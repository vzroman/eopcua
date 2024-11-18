%%----------------------------------------------------------------
%% Copyright (c) 2021 Faceplate
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%----------------------------------------------------------------

-ifndef(EOPCUA_STRUCT).
-define(EOPCUA_STRUCT,1).

-ifndef(TEST).

-define(LOGERROR(Text),logger:error(Text)).
-define(LOGERROR(Text,Params),logger:error(Text,Params)).
-define(LOGWARNING(Text),logger:warning(Text)).
-define(LOGWARNING(Text,Params),logger:warning(Text,Params)).
-define(LOGINFO(Text),logger:info(Text)).
-define(LOGINFO(Text,Params),logger:info(Text,Params)).
-define(LOGDEBUG(Text),logger:debug(Text)).
-define(LOGDEBUG(Text,Params),logger:debug(Text,Params)).

-else.

-define(LOGERROR(Text),ct:pal("error: "++Text)).
-define(LOGERROR(Text,Params),ct:pal("error: "++Text,Params)).
-define(LOGWARNING(Text),ct:pal("warning: "++Text)).
-define(LOGWARNING(Text,Params),ct:pal("warning: "++Text,Params)).
-define(LOGINFO(Text),ct:pal("info: "++Text)).
-define(LOGINFO(Text,Params),ct:pal("info: "++Text,Params)).
-define(LOGDEBUG(Text),ct:pal("debug: "++Text)).
-define(LOGDEBUG(Text,Params),ct:pal("debug: "++Text,Params)).

-endif.

-endif.
