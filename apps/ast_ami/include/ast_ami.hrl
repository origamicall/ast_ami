

-define(MSGS_DIR, "msgs").

-define(DU(F),io:format("~p~n",[F])).
-define(DO(F),io:format("~p :~p ::~s =>  ~p~n",[?MODULE,?LINE,??F,F])).


%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
   logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
   logger:info_msg(?MODULE,?LINE,Format, Args)).

-define(WARNING_MSG(Format, Args),
   logger:warning_msg(?MODULE,?LINE,Format, Args)).

-define(ERROR_MSG(Format, Args),
   logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
   logger:critical_msg(?MODULE,?LINE,Format, Args)).
