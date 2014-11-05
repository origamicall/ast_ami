%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%%-------------------------------------------------------------------
%%% File    : ast_man_sup.erl
%%% Author  : Anders Nygren <anders@telteq.com.mx>
%%% Description : 
%%%
%%% Created : 29 Mar 2006 by Anders Nygren <anders@telteq.com.mx>
%%%-------------------------------------------------------------------
%% @private

-module(ast_ami_sup).

-behaviour(supervisor).

-include("ast_ami.hrl").
%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	Pars = case application:get_env(ast_ami, asterisk) of
		{ok, Data} ->
			[proplists:get_value("ip", Data),
			 proplists:get_value("port", Data),
			 proplists:get_value("user", Data),
			 proplists:get_value("passw", Data)];
		_Nothing ->
			[{127,0,0,1},5038,"mark","mysecret"]
		end,
		AstEvt = {ast_evt,{ast_man_events,start_link,[]},
	    	permanent,2000,worker,[ast_man_events]},
		AstMan = {ast_man,{ast_ami_client,start_link,[]},
			permanent,20,worker,[ast_ami_client]},
		AstDrv = {ast_drv,{ast_ami_drv,start_link,Pars},
	      	permanent,2000,worker,[ast_ami_drv]},
	    {ok,{{one_for_all,10000,2000}, [AstDrv,AstEvt,AstMan]}}.
		
	

%init(StartArgs) -> 
%	AstEvt = {ast_evt,{ast_man_events,start_link,[]},
%		permanent,2000,worker,[ast_man_events]},
%	AstMan = {ast_man,{ast_ami_client,start_link,[]},
%		permanent,2000,worker,[ast_ami_client]},
%	AstDrv = {ast_drv,{ast_ami_drv,start_link,StartArgs},
%		permanent,2000,worker,[ast_ami_drv]},
%    {ok,{{one_for_all,100,1000}, [AstDrv,AstEvt,AstMan]}}.

%%====================================================================
%% Internal functions
%%====================================================================
