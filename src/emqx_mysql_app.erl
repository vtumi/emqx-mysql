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

-module(emqx_mysql_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , prep_stop/1
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_mysql_sup:start_link(),
    register_metrics(),
    load(application:get_all_env()),
    emqx_mysql_cfg:register(),
    {ok, Sup}.

prep_stop(State) ->
    unload(),
    emqx_mysql_cfg:unregister(),
    State.

stop(_State) ->
    ok.

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['mysql.auth.success',
                                                    'mysql.auth.failure',
                                                    'mysql.auth.ignore',
                                                    'mysql.acl.allow',
                                                    'mysql.acl.deny',
                                                    'mysql.acl.ignore',
                                                    'mysql.client.connected',
                                                    'mysql.client.disconnected',
                                                    'mysql.message.publish']].

load(Env) ->
    emqx:hook('client.authenticate', fun emqx_mysql:on_client_authenticate/2, [Env]),
    emqx:hook('client.check_acl', fun emqx_mysql_acl:on_client_check_acl/5, [Env]),
    emqx:hook('client.connected', fun emqx_mysql:on_client_connected/4, [Env]),
    emqx:hook('client.disconnected', fun emqx_mysql:on_client_disconnected/3, [Env]),
    emqx:hook('message.publish', fun emqx_mysql:on_message_publish/2, [Env]).

unload() ->
    emqx:unhook('client.authenticate', fun emqx_mysql:on_client_authenticate/2),
    emqx:unhook('client.check_acl', fun emqx_mysql_acl:on_client_check_acl/5),
    emqx:unhook('client.connected', fun emqx_mysql:on_client_connected/4),
    emqx:unhook('client.disconnected', fun emqx_mysql:on_client_disconnected/3),
    emqx:unhook('message.publish', fun emqx_mysql:on_message_publish/2).
