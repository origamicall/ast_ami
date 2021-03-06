%%%-------------------------------------------------------------------
%%% File    : ast_ami_app.erl
%%% @copyright 2007 Anders Nygren <anders.nygren@gmail.com>
%%% @version  {@vsn} 
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%% Created : 11 Mar 2007 by Anders Nygren <anders@>
%%%-------------------------------------------------------------------
-module(ast_ami_app).

-behaviour(application).

-include("ast_ami.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
	{Status, Pid} = case ast_ami_sup:start_link(StartArgs) of
		{ok,P} -> 
			{ok, P};
		{error, St} ->
			{error, St}
	end,
	server_nodes_sup:start_link([]),
	{Status, Pid}.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

