

-define(MSGS_DIR, "msgs").

-define(DU(F),io:format("~p~n",[F])).
-define(DO(F),io:format("~p :~p ::~s =>  ~p~n",[?MODULE,?LINE,??F,F])).


%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
   lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
   lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
   lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
   lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
   lager:critical(Format, Args)).
