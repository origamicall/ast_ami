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
%%% @copyright 2006 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Pack and unpack pdus.
%%% @end 
%%% Created : 10 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_ami_pdu).

%% @headerfile "manager_api.hrl"
-include("manager_api.hrl").
-include("ast_ami.hrl").

%% API
-export([encode/1, parse/1, parse_action/2,
   get_pdu_var/2, 
   pdu_type/1, 
   pdu_type_action/2, 
   pdu_type_event/2, 
   pdu_type_alarm/2,
   parse_event/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec encode(Cmd::ami_cmd()) -> io_list()
%% @doc Create a binary pdu from a command record.
%% @end
%%--------------------------------------------------------------------
encode(#absolute_timeout{channel=Channel,timeout=Timeout} ) ->
    pack_pdu([{"Action","AbsoluteTimeout"},
	      {"Channel",Channel},
	      {"Timeout",Timeout}]);

encode(#agent_callback_login{agent=Agent,
				 exten=Exten,
				 context=Context,
				 ackCall=AckCall,
				 wrapUpTime=WrapUpTime} ) ->
    pack_pdu([{"Action","AgentCallBackLogin"},
	      {"Agent",Agent},
	      {"Exten",Exten},
	      {"Context",Context},
	      {"AckCall",AckCall},
	      {"WrapUpTime",WrapUpTime}]);

encode( #agent_logoff{agent=Agent,soft=Soft}) ->
    pack_pdu([{"Action","AgentLogoff"},
	      {"Agent",Agent},
	      {"soft",Soft}]);

encode(#agents{} ) ->
    pack_pdu([{"Action","Agents"}]);

encode(#logoff{}) ->
	pack_pdu([{"Action","Logoff"}]);

encode(#challenge{authtype=AuthType}) ->
    pack_pdu([{"Action","Challenge"},
	      {"AuthType",AuthType}]);

encode(#change_monitor{channel=Channel,file=File} ) ->
    pack_pdu([{"Action","ChangeMonitor"},
	      {"Channel",Channel},
	      {"File",File}]);

encode(#command{command=Command} ) ->
	%?DEBUG("Command :~p ",[Command]),
    pack_pdu([{"Action","Command"},
	      {"Command",Command}]);

encode(#db_del{family=Family,key=Key} ) ->
    pack_pdu([{"Action","DBDel"},
	      {"Family",Family},
	      {"Key",Key }]);

encode(#db_get{family=Family,key=Key} ) ->
    pack_pdu([{"Action","DBGet"},
	      {"Family",Family},
	      {"Key",Key }]);

encode(#db_put{family=Family,key=Key,value=Value} ) ->
    pack_pdu([{"Action","DBPut"},
	      {"Family",Family},
	      {"Key",Key},
	      {"Value",Value}]);

encode(#events{eventMask=EventMask} ) ->
    pack_pdu([{"Action","Events"},
	      {"EventMask",EventMask}]);

encode(#extension_state{extension=Extension,context=Context} ) ->
    pack_pdu([{"Action","ExtensionState"},
	      {"Extension",Extension},
	      {"Context",Context}]);

encode(#get_var{channel=Channel,var=Var}) ->
    pack_pdu([{"Action","GetVar"},
	      {"Channel",Channel},
	      {"Var",Var}]);

encode(#hangup{channel=Channel}) ->
    pack_pdu([{"Action","Hangup"},
	      {"Channel",Channel}]);

encode(#iax_netstats{} ) ->
    pack_pdu([{"Action","IAXnetstats"}]);

encode(#iax_peers{} ) ->
    pack_pdu([{"Action","IAXPeers"}]);

encode(#list_commands{} ) ->
    pack_pdu([{"Action","ListCommands"}]);

encode(#login{user=User,secret=Secret}) ->
    pack_pdu([{"Action","login"},
	      {"Username",User},
	      {"Secret",Secret},
		  {"Events","on"}]);

%encode(#mailbox_count{mailbox=Mbox}) ->
%    pack_pdu([{"Action","MailboxCount"},
%	      {"Mailbox",Mbox}]);

encode(#mailbox_status{mailbox=Mailbox}) ->
    pack_pdu([{"Action","MailboxStatus"},
	      {"Mailbox",Mailbox}]);

encode(#monitor{channel=Channel,file=File,format=Format,mix=Mix}) ->
    pack_pdu([{"Action","Monitor"},
	      {"Channel",Channel},
	      {"File",File},
	      {"Format",Format},
	      {"Mix",Mix}]);

encode(#originate{channel=Channel,extension=Extension,context=Context,priority=Priority,application=Application,data=Data,timeout=Timeout,callerid=CallerID,vars=Vars,account=Account,async=Async}) ->
	?INFO_MSG("Recibiendo informacion del Originate: Channel: ~p | Exten: ~p | Context: ~p | Priority: ~p " ++
		" Application: ~p | Data: ~p | Timeout: ~p | CallerId: ~p | Vars: ~p | Acct: ~p | Async: ~p",
		[Channel, Extension, Context, Priority, Application, Data, Timeout, CallerID, Vars, Account, Async]),
    pack_pdu([{"Action","Originate"},
	      {"Channel",Channel},
		  {"Exten", Extension},
		  {"Context", Context},
		  {"Priority", Priority},
	      {"Application", Application},
	      {"Data", Data},
		  {"Variable", Vars},
	      {"Timeout",integer_to_list(Timeout)},
	      {"CallerId", CallerID},
	      {"Account", Account},
	      {"Async", atom_to_list(Async)}
		  ]);

%&encode(#originate{channel=Channel,extension=Extension,context=Context,priority=Priority,application=Application,data=Data,timeout=Timeout,callerid=CallerID,vars=Vars,account=Account,async=Async}) ->
%    pack_pdu([{"Action","Originate"},
%          {"Channel",Channel},
%		  {"Exten", Extension},
%		  {"Context", Context},
%		  {"Priority", Priority},
%		  {"Application", Application},
%		  {"Data", Data},
%		  {"Variable", Vars},
%	      {"Timeout",list_to_Timeout},
%	      {"CallerId", CallerID},
%	      {"Account", Account},
%	      {"Async", atom_to_list(Async)}]);


encode(#parked_calls{}) ->
    pack_pdu([{"Action","ParkedCalls"}]);

encode(#ping{}) ->
    pack_pdu([{"Action","Ping"}]);

encode(#queue_add{interface=Interface,
		      queue=Queue,
		      penalty=Penalty,
		      pause=Pause}) ->
    pack_pdu([{"Action","QueueAdd"},
	      {"Interface",Interface},
	      {"Queue",Queue},
	      {"Penalty",Penalty},
	      {"Pause",Pause}]);

encode(#queue_pause{interface=Interface,
			queue=Queue,
			pause=Pause}) ->
    pack_pdu([{"Action","QueuePause"},
	      {"Interface",Interface},
	      {"Queue",Queue},
	      {"Pause",Pause}]);

encode(#queue_remove{interface=Interface,
			 queue=Queue}) ->
    pack_pdu([{"Action","QueueRemove"},
	      {"Interface",Interface},
	      {"Queue",Queue}]);

encode(#queues{}) ->
    pack_pdu([{"Action","Queues"}]);

encode(#queue_status{}) ->
    pack_pdu([{"Action","QueueStatus"}]);


encode(#redirect{channel=Channel,
		     extrachannel=ExtraChannel,
		     exten=Exten,
		     context=Context,
		     priority=Priority}) ->
    pack_pdu([{"Action","Redirect"},
	      {"Channel",Channel},
	      {"Extrachannel",ExtraChannel},
	      {"Exten",Exten},
	      {"Context",Context},
	      {"Priority",Priority}]);

encode(#set_cdr_user_field{channel=Channel,
			       userfield=Userfield,
			       append=Append}) ->
    pack_pdu([{"Action","SetCDRUserField"},
	      {"Channel",Channel},
	      {"Userfield",Userfield},
	      {"Append",Append}]);

encode(#set_var{channel=Channel,
		    variable=Variable,
		    value=Value}) ->
    pack_pdu([{"Action","SetVar"},
	      {"Channel",Channel},
	      {"Variable",Variable},
	      {"Value",Value}]);

encode(#sip_showpeer{peer=Peer}) ->
    pack_pdu([{"Action","SIPshowpeer"},
	      {"Peer",Peer}]);

encode(#sip_peers{}) ->
    pack_pdu([{"Action","SIPpeers"}]);

encode(#status{}) ->
    pack_pdu([{"Action","Status"}]);

encode(#stop_monitor{channel=Channel}) ->
    pack_pdu([{"Action","StopMonitor"},
	      {"Channel",Channel}]);

encode(#zap_dial_offhook{channel=Channel,
			     number=Number}) ->
    pack_pdu([{"Action","ZapDialOffhook"},
	      {"Channel",Channel},
	      {"Number",Number}]);

encode(#zap_dnd_off{channel=Channel}) ->
    pack_pdu([{"Action","ZapDNDOff"},
	      {"Channel",Channel}]);

encode(#zap_dnd_on{channel=Channel}) ->
    pack_pdu([{"Action","ZapDNDOn"},
	      {"Channel",Channel}]);

encode(#zap_hangup{channel=Channel}) ->
    pack_pdu([{"Action","ZapHangup"},
	      {"Channel",Channel}]);

encode(#zap_show_channels{}) ->
    pack_pdu([{"Action","ZapShowChannels"}]).

%%--------------------------------------------------------------------
%% @spec parse_action(Action::string(),Pars::list()) -> :ami_cmd()
%% @doc Create an action request record from a .
%% @end
%%--------------------------------------------------------------------
parse_action("AbsoluteTimeout",Pdu) ->
    #absolute_timeout{channel=get_pdu_var("Channel",Pdu),
			  timeout=get_pdu_var("Timeout",Pdu)};
parse_action("AgentCallbackLogin",Pdu) ->
    #agent_callback_login{agent=get_pdu_var("Agent",Pdu),
			      exten=get_pdu_var("Exten",Pdu),
			      context=get_pdu_var("Context",Pdu),
			      ackCall=get_pdu_var("AckCall",Pdu),
			      wrapUpTime=get_pdu_var("WrapUpTime",Pdu)};
parse_action("AgentLogoff",Pdu) ->
    #agent_logoff{agent=get_pdu_var("Agent",Pdu),
		      soft=get_pdu_var("Soft",Pdu)};


parse_action("Logoff",_Pdu) ->
	#logoff{};

parse_action("Agents",_Pdu) ->
    #agents{};

parse_action("ChangeMonitor",Pdu) ->
    #change_monitor{channel=get_pdu_var("Channel",Pdu),
			file=get_pdu_var("File",Pdu)};

parse_action("Command",Pdu) ->
    #command{command=get_pdu_var("Command",Pdu)};

parse_action("DBGet",Pdu) ->
    #db_get{family=get_pdu_var("Family",Pdu),
		key=get_pdu_var("Key",Pdu)};

parse_action("DBPut",Pdu) ->
    #db_put{family=get_pdu_var("Family",Pdu),
		key=get_pdu_var("Key",Pdu),
		value=get_pdu_var("Value",Pdu)};

parse_action("Events",Pdu) ->
    #events{eventMask=get_pdu_var("EventMask",Pdu)};

parse_action("ExtensionState",Pdu) ->
    #extension_state{extension=get_pdu_var("Extension",Pdu),
			 context=get_pdu_var("Context",Pdu)};

parse_action("GetVar",Pdu) ->
    #get_var{channel=get_pdu_var("Channel",Pdu),
		 var=get_pdu_var("Var",Pdu)};

parse_action("Hangup",Pdu) ->
    #hangup{channel=get_pdu_var("Channel",Pdu)};

parse_action("IAXNetStats",_Pdu) ->
    #iax_netstats{};

parse_action("IAXPeers",_Pdu) ->
    #iax_peers{};

parse_action("ListCommands",_Pdu) ->
    #list_commands{};

parse_action("Login",Pdu) ->
    #login{user=get_pdu_var("User",Pdu),
	       secret=get_pdu_var("Secret",Pdu)};

%parse_action("MailboxCount",Pdu) ->
%    #mailbox_count{mailbox=get_pdu_var("Mailbox",Pdu),
%		       new=get_pdu_var("NewMessages",Pdu),
%		       old=get_pdu_var("OldMessages",Pdu)};

parse_action("MailboxStatus",Pdu) ->
    #mailbox_status{mailbox=get_pdu_var("Mailbox",Pdu),
			waiting=get_pdu_var("Waiting",Pdu)};

parse_action("Monitor",Pdu) ->
    #monitor{channel=get_pdu_var("Channel",Pdu),
		 file=get_pdu_var("File",Pdu),
		 format=get_pdu_var("Format",Pdu),
		 mix=get_pdu_var("Mix",Pdu)};

parse_action("Originate",Pdu) ->
    #originate{channel=get_pdu_var("Channel",Pdu)};

parse_action("ParkedCalls",_Pdu) ->
    #parked_calls{};

parse_action("Ping",_Pdu) ->
    #ping{};

parse_action("QueueAdd",Pdu) ->
    #queue_add{interface=get_pdu_var("Interface",Pdu),
		   queue=get_pdu_var("Queue",Pdu),
		   penalty=get_pdu_var("Penalty",Pdu),
		   pause=get_pdu_var("Pause",Pdu)};

parse_action("QueuePause",Pdu) ->
    #queue_pause{interface=get_pdu_var("Interface",Pdu),
		     queue=get_pdu_var("Queue",Pdu),
		     penalty=get_pdu_var("Penalty",Pdu),
		     pause=get_pdu_var("Pause",Pdu)};

parse_action("QueueRemoveInterface",Pdu) ->
    #queue_remove{interface=get_pdu_var("Interface",Pdu),
		      queue=get_pdu_var("Queue",Pdu)};

parse_action("Queues",_Pdu) ->
    #queues{};

parse_action("QueueStatus",_Pdu) ->
    #queue_status{};

parse_action("Redirect",Pdu) ->
    #redirect{channel=get_pdu_var("Channel",Pdu),
		  extrachannel=get_pdu_var("ExtraChannel",Pdu),
		  exten=get_pdu_var("Exten",Pdu),
		  context=get_pdu_var("",Pdu),
		  priority=get_pdu_var("",Pdu)};

parse_action("set_cdr_user_field",Pdu) ->
    #set_cdr_user_field{channel=get_pdu_var("Channel",Pdu),
			    userfield=get_pdu_var("UserField",Pdu),
			    append=get_pdu_var("Append",Pdu)};

parse_action("SetVar",Pdu) ->
    #set_var{channel=get_pdu_var("Channel",Pdu),
		 variable=get_pdu_var("Variable",Pdu),
		 value=get_pdu_var("Value",Pdu)};

parse_action("SIPshowpeer",Pdu) ->
    #sip_showpeer{peer=get_pdu_var("Peer",Pdu)};

parse_action("SIPpeers",_Pdu) ->
    #sip_peers{};

parse_action("Status",Pdu) ->
    #status{channel=get_pdu_var("Channel",Pdu)};

parse_action("StopMonitor",Pdu) ->
    #stop_monitor{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapDialOffhook",Pdu) ->
    #zap_dial_offhook{channel=get_pdu_var("Channel",Pdu),
			  number=get_pdu_var("Number",Pdu)};

parse_action("ZapDndOff",Pdu) ->
    #zap_dnd_off{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapDndOn",Pdu) ->
    #zap_dnd_on{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapHangup",Pdu) ->
    #zap_hangup{channel=get_pdu_var("Channel",Pdu)};

parse_action("ZapShowChannels",_Pdu) ->
    #zap_show_channels{};

parse_action("ZapTransfer",_Pdu) ->
    #zap_transfer{}.

%%--------------------------------------------------------------------
%% @spec parse(Data::string()) -> {[Pdu::pdu()],Cont::string()}
%% @doc Parse a string() containing 0 or more pdus.
%% @end
%%--------------------------------------------------------------------
parse(Data) ->
    {Pdus,Cont} = parse(Data,[]).

%%====================================================================
%% Internal functions
%%====================================================================
parse(Data,Acc) ->
    case split(Data) of
	{nothing,Rows} ->
	    {lists:reverse(Acc),Rows};
	{Msg,More} ->
	    parse(More,[Msg|More])
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

get_pdu_var(Lbl,Pdu) ->
    case lists:keysearch(Lbl,1,Pdu) of
	{value,{Lbl,Val}} ->
	    Val;
	false ->
	    undefined
    end.

pdu_type(Pdu) ->
    pdu_type_action(Pdu,get_pdu_var("Action",Pdu)).

pdu_type_action(Pdu,undefined) ->
    pdu_type_event(Pdu,get_pdu_var("Event",Pdu));
pdu_type_action(Pdu,Action) ->
    {action,Action,Pdu}.

pdu_type_event(Pdu,undefined) ->
    pdu_type_alarm(Pdu,get_pdu_var("Alarm",Pdu));
pdu_type_event(Pdu,Event) ->
    {event,Event,Pdu}.

pdu_type_alarm(Pdu,undefined)->
    exit({unknown_pdu_type,Pdu});
pdu_type_alarm(Pdu,Alarm) ->
    {alarm,Alarm,Pdu}.

pack_pdu(Ls) ->
    [pack_l(L) || L <- Ls].

pack_l({L,V}) when is_integer(V) ->
    [list_to_binary(L),<<": ">>,
     list_to_binary(integer_to_list(V)),
     <<"\r\n">>];
pack_l({L,V}) when is_atom(V) ->
    [list_to_binary(L),<<": ">>,list_to_binary(atom_to_list(V)),<<"\r\n">>];
pack_l({L,V}) when is_list(V) ->
    [list_to_binary(L),<<": ">>,list_to_binary(V),<<"\r\n">>].



parse_event(_Alarm,_Pdu) ->
    ok.
