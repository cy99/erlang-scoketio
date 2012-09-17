%% Author: Administrator
%% Created: 2012-9-17
%% Description: TODO: Add description to encoding_tests
-module(encoding_tests).
-define()


Msg = "0::/test".
re:run(Msg, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?", [{capture, all_but_first, list}]).
Msg = "5:1+:/chat:{\"name\":\"nickname\",\"args\":[\"firefox\"]}".


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

