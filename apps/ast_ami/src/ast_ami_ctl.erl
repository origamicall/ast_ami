
-module(ast_ami_ctl).

-export([start/0, start/1,
	 init/0,
	 process/1,
	 dump_to_textfile/1,
	 register_commands/3,
	 register_commands/4,
	 unregister_commands/3,
	 unregister_commands/4,
	 print_vhost_usage/1 ]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

-include("ast_ami.hrl").

start() ->
	start([]).

start([]) ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    SNode1 = case string:tokens(SNode, "@") of
		[_Node, _Server] ->
		    SNode;
		_ ->
		    case net_kernel:longnames() of
			 true ->
			     SNode ++ "@" ++ inet_db:gethostname() ++
				      "." ++ inet_db:res_option(domain);
			 false ->
			     SNode ++ "@" ++ inet_db:gethostname();
			 _ ->
			     SNode
		     end
	    end,
	    Node = list_to_atom(SNode1),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
    ets:new(ast_ami_ctl_cmds, [named_table, set, public]),
    ets:new(ast_ami_ctl_host_cmds, [named_table, set, public]).


process(["reopen-log"]) ->
	ast_logger_h:reopen_log(),
	?STATUS_SUCCESS;

process(_Args) ->
	print_usage(),
	?STATUS_USAGE.


print_usage() ->
    CmdDescs =
	[{"reopen-log", "reopen log file"}] ,
	%++	ets:tab2list(ast_ami_ctl_cmds),
	MaxCmdLen =
	lists:max(lists:map(
		fun({Cmd, _Desc}) ->
			length(Cmd)
		end, CmdDescs)),
	NewLine = io_lib:format("~n", []),
	FmtCmdDescs =
	lists:map(
		fun({Cmd, Desc}) ->
			["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
			Desc, NewLine]
		end, CmdDescs),
	?PRINT(
		"Usage: ast_ami [--node nodename] command [options]~n"
		"~n"
		"Available commands in this ast_ami node:~n"
		++ FmtCmdDescs ++
		"~n"
		"Examples:~n"
		"  erlastctl reopen-log~n",
	[]).

print_vhost_usage(Host) ->
    CmdDescs =
	ets:select(ast_ami_ctl_host_cmds,
		   [{{{Host, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]),
    MaxCmdLen =
	if
	    CmdDescs == [] ->
		0;
	    true ->
		lists:max(lists:map(
			    fun({Cmd, _Desc}) ->
				    length(Cmd)
			    end, CmdDescs))
	end,
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: ast_ami [--node nodename] vhost hostname command [options]~n"
      "~n"
      "Available commands in this ast_ami node and this vhost:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  erlastctl vhost "++Host++" registered-users~n",
     []).

register_commands(CmdDescs, Module, Function) ->
    ets:insert(ast_ami_ctl_cmds, CmdDescs),
    ast_ami_hooks:add(ast_ami_ctl_process,
		       Module, Function, 50),
    ok.

register_commands(Host, CmdDescs, Module, Function) ->
    ets:insert(ast_ami_ctl_host_cmds,
	       [{{Host, Cmd}, Desc} || {Cmd, Desc} <- CmdDescs]),
    ast_ami_hooks:add(ast_ami_ctl_process, Host,
		       Module, Function, 50),
    ok.

unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
			  ets:delete_object(ast_ami_ctl_cmds, CmdDesc)
		  end, CmdDescs),
    ast_ami_hooks:delete(ast_ami_ctl_process,
			  Module, Function, 50),
    ok.

unregister_commands(Host, CmdDescs, Module, Function) ->
    lists:foreach(fun({Cmd, Desc}) ->
			  ets:delete_object(ast_ami_ctl_host_cmds,
					    {{Host, Cmd}, Desc})
		  end, CmdDescs),
    ast_ami_hooks:delete(ast_ami_ctl_process,
			  Module, Function, 50),
    ok.

dump_to_textfile(File) ->
	dump_to_textfile(mnesia:system_info(is_running), file:open(File, write)).

dump_to_textfile(yes, {ok, F}) ->
	Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
	Tabs = lists:filter(
		fun(T) ->
			case mnesia:table_info(T, storage_type) of
			disc_copies -> true;
			disc_only_copies -> true;
			_ -> false
		end
	end, Tabs1),
	Defs = lists:map(
		fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			{attributes, mnesia:table_info(T, attributes)}]} 
		end,
	Tabs),
	io:format(F, "~p.~n", [{tables, Defs}]),
	lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
	file:close(F);

dump_to_textfile(_, {ok, F}) ->
	file:close(F),
	{error, mnesia_not_running};
dump_to_textfile(_, {error, Reason}) ->
	{error, Reason}.


dump_tab(F, T) ->
	W = mnesia:table_info(T, wild_pattern),
	{atomic,All} = mnesia:transaction(
		fun() -> mnesia:match_object(T, W, read) end),
			lists:foreach(
			fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

