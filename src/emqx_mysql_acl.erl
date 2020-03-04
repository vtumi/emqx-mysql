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

-module(emqx_mysql_acl).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(DEVICE_ACL_SQL, <<"select `allow`, `ipaddr`, `username`, `access`, `topic` from `mqtt_acl` where `ipaddr` = ? or `username` = ? or `username` = '$all'">>).

-export([on_client_check_acl/5, reload_acl/1, description/0]).

on_client_check_acl(Credentials, PubSub, Topic, NoMatchAction, State) ->
  case do_check_acl(Credentials, PubSub, Topic, NoMatchAction, State) of
    ok -> emqx_metrics:inc('mysql.acl.ignore'), ok;
    {stop, allow} -> emqx_metrics:inc('mysql.acl.allow'), {stop, allow};
    {stop, deny} -> emqx_metrics:inc('mysql.acl.deny'), {stop, deny}
  end.

do_check_acl(#{username := <<$$, _/binary>>}, _PubSub, _Topic, _NoMatchAction, _State) ->
  ok;
do_check_acl(Credentials = #{username := Username, peername := {Peerhost, _}}, PubSub, Topic, _NoMatchAction, _State) ->
  case emqx_mysql_cli:query(?DEVICE_ACL_SQL, [iolist_to_binary(inet_parse:ntoa(Peerhost)), Username]) of
    {ok, _Columns, []} -> ok;
    {ok, _Columns, Rows} ->
      Rules = filter(PubSub, compile(Rows)),
      case match(Credentials, Topic, Rules) of
        {matched, allow} -> {stop, allow};
        {matched, deny} -> {stop, deny};
        nomatch -> ok
      end;
    {error, Reason} ->
      ?LOG(error, "[MySQL] do_check_acl error: ~p~n", [Reason]),
      ok
  end.

match(_Credentials, _Topic, []) ->
  nomatch;

match(Credentials, Topic, [Rule|Rules]) ->
  case emqx_access_rule:match(Credentials, Topic, Rule) of
    nomatch ->
      match(Credentials, Topic, Rules);
    {matched, AllowDeny} ->
      {matched, AllowDeny}
  end.

filter(PubSub, Rules) ->
  [Term || Term = {_, _, Access, _} <- Rules,
   Access =:= PubSub orelse Access =:= pubsub].

compile(Rows) ->
  compile(Rows, []).
compile([], Acc) ->
  Acc;
compile([[Allow, IpAddr, Username, Access, Topic]|T], Acc) ->
  Who = who(IpAddr, Username),
  Term = {allow(Allow), Who, access(Access), [topic(Topic)]},
  compile(T, [emqx_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>) ->
  all;
who(null, null) ->
  throw(undefined_who);
who(CIDR, Username) ->
  Cols = [{ipaddr, b2l(CIDR)}, {user, Username}],
  case [{C, V} || {C, V} <- Cols, not empty(V)] of
    [Who] -> Who;
    Conds -> {'and', Conds}
  end.

allow(1)  -> allow;
allow(0)  -> deny;
allow(<<"1">>)  -> allow;
allow(<<"0">>)  -> deny.

access(1) -> subscribe;
access(2) -> publish;
access(3) -> pubsub;
access(<<"1">>) -> subscribe;
access(<<"2">>) -> publish;
access(<<"3">>) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
  {eq, Topic};
topic(Topic) ->
  Topic.

reload_acl(_State) ->
  ok.

description() ->
  "ACL with Mysql".

b2l(null) -> null;
b2l(B)    -> binary_to_list(B).

empty(null) -> true;
empty("")   -> true;
empty(<<>>) -> true;
empty(_)    -> false.
