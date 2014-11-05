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
%%% File    : ast_ami_drv.erl
%%% Author  : anders <anders.nygren@gmail.com>
%%% Description :
%%%
%%% Created : 11 Mar 2007 by anders <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_ami_drv).


-behaviour(gen_server).

-include("ast_ami.hrl").
%% API
-export([start_link/4,start_link/5, send/1]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3,
	 stop/0,
	 parse/1,
	 split/1,
	 split_line/1,
	 list_convert/1
	]).

-define(SERVER,?MODULE).


-record(state, {from,aid=0,sock,acc=[],ip,port,user,passwd}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(IPaddr,Port,User,Passwd) ->
%%     gen_server:start_link({local, ?SERVER}, ?MODULE,
%% 			  [IPaddr,Port,User,Passwd], []).
    gen_server:start_link({local, ?SERVER}, ?MODULE,
% 			  [IPaddr,Port,User,Passwd], [{debug,[trace]}]).
			  [IPaddr,Port,User,Passwd], []).

start_link(Name,IPaddr,Port,User,Passwd) ->
%%  gen_server:start_link(Name, ?MODULE, [IPaddr,Port,User,Passwd], []).
%	gen_server:start_link(Name, ?MODULE, [IPaddr,Port,User,Passwd], [{debug,[trace]}]).
	gen_server:start_link(Name, ?MODULE, [IPaddr,Port,User,Passwd], []).

send(Request) ->
	gen_server:call(?SERVER,{api,Request}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([IPaddr,Port,User,Passwd]) ->
    {ok,Sock} = gen_tcp:connect(IPaddr,Port,[list,{packet,line}]),
    {ok, #state{sock=Sock,ip=IPaddr,port=Port,user=User,passwd=Passwd}}.

%%--------------------------------------------------------------------
%% @private
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({api,Request}, _From, State) ->
    Aid=State#state.aid,
    Req=add_aid(Request,Aid),
    case (catch send(State,Req)) of 
		{'EXIT',_} ->
			ast_ami_drv:stop(),
			?ERROR_MSG("Detectando caida de asterisk enviando depuracion", []),
			acd_nodes:depura_acd(),
			voyeur_nodes:depura_voyeur();
		Vars ->
			Vars
	end,
    {reply, Aid, State#state{aid=Aid+1}}.

%%--------------------------------------------------------------------
%% @private
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp,_Sock,"Asterisk Call Manager"++_More}, State) ->
    {noreply, State};

handle_info({tcp,_Sock,"\r\n"}, State) ->
    ast_ami_client:new_pdu([{"EventTime",erlang:now()}|
		lists:reverse(State#state.acc)]),
    {noreply, State#state{acc=[]}};

handle_info({tcp,_Sock,Line}, State) ->
    L1 = string:strip(hd(case string:tokens(Line,"\r\n") of
    		[] -> ["asasasas"];
    		R -> R
    	end)),
    Acc=State#state.acc,
    {noreply, State#state{acc=[L1|Acc]}};

handle_info({tcp_close,_Sock,_Data}, State) ->

    {noreply, State#state{sock=undefined}};

handle_info(_Info, State) ->
    {noreply, State}.


stop() ->
    gen_server:cast(?MODULE, stop).


%%--------------------------------------------------------------------
%% @private
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

add_aid(Request,Aid) ->
    [Request,"ActionId: ",integer_to_list(Aid),"\r\n\r\n"].

send(State,Pack) ->
	Sock = State#state.sock,
	% TODO: hacer un catch para el timeout
	case gen_tcp:send(Sock,Pack) of
		ok ->
			ok
			% en caso de que no haya salido bien, dejamos que
			% genere el error para que el supervisor lo reinicie
			% todo.
	end.

parse(Data) ->
    case split(Data) of
	{nothing,Rest} ->
	    Rest;
	{Msg,More} ->
%%	    io:format("sending to manager: ~w~n",[lists:flatten(Msg)]),
	    ast_manager:new_pdu([{"EventTime",erlang:now()}|Msg]),
	    parse(More)
    end.

split(Data) ->
    case string:str(Data,"\r\n\r\n") of
	Pos when Pos>0 ->
	    Msg=string:substr(Data,1,Pos-1),
	    Msg1=string:tokens(Msg,"\r\n"),
	    Msg2=[split_line(L)|| L<- Msg1],
	    More=string:substr(Data,Pos+4),
	    {Msg2,More};
	_NotFound ->
	    {nothing,Data}
    end.
%%

split_line(L) ->
    Pos=string:str(L,": "),
    Lbl=string:substr(L,1,Pos-1),
    Value=string:substr(L,Pos+2),
    {Lbl,Value}.


list_convert(Dato) ->
	if 
		erlang:is_list(Dato) ->
			Dato;
		erlang:is_integer(Dato) ->
			erlang:integer_to_list(Dato);
		erlang:is_tuple(Dato) ->
			erlang:tuple_to_list(Dato);
		erlang:is_atom(Dato) ->
			erlang:atom_to_list(Dato);
		erlang:is_binary(Dato)->
			erlang:binary_to_list(Dato);
		true ->
			""
	end.


