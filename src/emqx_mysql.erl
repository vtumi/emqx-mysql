%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_mysql).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(DEVICE_AUTH_SQL, <<"select `password` from `mqtt_device` where `username` = ? limit 1">>).
-define(DEVICE_SUPER_SQL, <<"select `is_superuser` from `mqtt_device` where `username` = ? limit 1">>).
-define(DEVICE_ONLINE_SQL, <<"update `mqtt_device` set `state` = 1, `node` = ?, `ipaddr` = ?, `online_at` = ? where `username` = ?">>).
-define(DEVICE_OFFLINE_SQL, <<"update `mqtt_device` set `state` = 0, `offline_at` = ? where `username` = ?">>).
-define(DEVICE_MESSAGE_SQL, <<"insert into `mqtt_msg` (`mid`, `topic`, `sender`, `node`, `ipaddr`, `qos`, `retain`, `payload`, `create_at`) values (?, ?, ?, ?, ?, ?, ?, ?, ?)">>).

-export([description/0]).
-export([on_client_authenticate/2, on_client_connected/4, on_client_disconnected/3]).
-export([on_message_publish/2]).

on_client_authenticate(Credentials = #{username := Username, password := Password}, _Env) ->
  CheckPass = case emqx_mysql_cli:query(?DEVICE_AUTH_SQL, [Username]) of
    {ok, [<<"password">>], [[PassHash]]} ->
      check_pass({PassHash, Password});
    {ok, _Columns, []} ->
      {error, not_found};
    {error, Reason} ->
      ?LOG(error, "[MySQL] Auth from mysql failed: ~p", [Reason]),
      {error, not_found}
  end,
  case CheckPass of
    ok ->
      emqx_metrics:inc('mysql.auth.success'),
      {stop, Credentials#{is_superuser => is_superuser(Username),
                          anonymous => false,
                          auth_result => success}};
    {error, not_found} ->
      emqx_metrics:inc('mysql.auth.ignore'), ok;
    {error, ResultCode} ->
      ?LOG(error, "[MySQL] Auth from mysql failed: ~p", [ResultCode]),
      emqx_metrics:inc('mysql.auth.failure'),
      {stop, Credentials#{auth_result => ResultCode, anonymous => false}}
  end.

-spec(is_superuser(undefined | string()) -> boolean()).
is_superuser(undefined) -> false;
is_superuser(Username) ->
  case emqx_mysql_cli:query(?DEVICE_SUPER_SQL, [Username]) of
    {ok, [_Super], [[1]]} ->
      true;
    {ok, [_Super], [[_False]]} ->
      false;
    {ok, [_Super], []} ->
      false;
    {error, _Error} ->
      false
  end.

check_pass({PassHash, Password}) ->
  case string:equal(PassHash, Password) of
    true -> ok;
     false -> {error, not_authorized}
  end.

on_client_connected(#{username := Username, peername := {Peerhost, _}}, 0, ConnInfo, _Env) ->
  emqx_metrics:inc('mysql.client.connected'),
  emqx_mysql_cli:query(?DEVICE_ONLINE_SQL, [atom_to_binary(node(), utf8), iolist_to_binary(inet_parse:ntoa(Peerhost)), format_timestamp(maps:get(connected_at, ConnInfo)), Username]),
  ok;
on_client_connected(#{}, _ConnAck, _ConnInfo, _Env) ->
  ok.

on_client_disconnected(#{}, auth_failure, _Env) ->
  ok;
on_client_disconnected(Client, {shutdown, Reason}, Env) when is_atom(Reason) ->
  on_client_disconnected(Reason, Client, Env);
on_client_disconnected(#{username := Username}, Reason, _Env)
  when is_atom(Reason) ->
  emqx_metrics:inc('mysql.client.disconnected'),
  emqx_mysql_cli:query(?DEVICE_OFFLINE_SQL, [format_timestamp(os:timestamp()), Username]),
  ok;
on_client_disconnected(_, Reason, _Env) ->
  ?LOG(error, "Client disconnected, cannot encode reason: ~p", [Reason]),
  ok.

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #message{id = Id, topic = Topic, qos = Qos, payload = Payload, timestamp = Timestamp, headers = #{username := Username, peername := {Peerhost, _}}, flags = #{retain := Retain}}, _Env) ->
  if
    Qos > 0 ->
      Params = [ emqx_guid:to_hexstr(Id)
               , Topic
               , Username
               , atom_to_binary(node(), utf8)
               , iolist_to_binary(inet_parse:ntoa(Peerhost))
               , Qos
               , format_retain(Retain)
               , Payload
               , format_timestamp(Timestamp)
      ],
      emqx_mysql_cli:query(?DEVICE_MESSAGE_SQL, Params);
    true ->
      true
  end,
  {ok, Message}.

format_retain(Retain) ->
  case Retain of
    true -> 1;
    false -> 0
  end.

format_timestamp(TS) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(TS),
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])).

description() -> "Datastore with MySQL".
