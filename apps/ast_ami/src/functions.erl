
-module(functions).
-compile(export_all).

-include("ast_ami.hrl").



%% DEPRECATED
get_config_file(Module,Data) ->
	case application:get_env(Module,Data) of
		{ok, ConfigData} ->
			{ok,ConfigData};
		undefined -> 
			{error, "undefined"};
		{error, Reason} ->
			{error,Reason}
	end.


get_config(App, Par) ->
	case application:get_env(App, Par) of
		{ok, Config} -> Config;
		undefined ->
			io:format("ERROR!!!!!:  HAY UN PROBLEMA CON EL FICHERO DE CONFIGURACION [~p]~n", [Par]),
			exit(normal);
		Reason ->
			io:format("ERROR!!!! con el fichero de configuracion:~n App: ~p Par: ~p Reason: ~p ~n", [App, Par, Reason]),
			exit(normal)
	end.



obten_dato(Key,Lista) when is_list(Key) ->
    obten_dato(list_to_atom(Key),Lista);

obten_dato(_Key,[]) ->
	"";

obten_dato(Key,Lista) ->
	[H|T] = Lista,
	case H of
		{Key,Value} -> Value;
	    _ -> obten_dato(Key,T)
	end.



to_bin(Dato)->
    case Dato of
	    null -> <<"">>;
	    _ -> binary_convert(Dato)
	end.


ifnull(Dato, Default) ->
	case Dato of
		undefined -> Default;
		null -> Default;
		false -> Default;
		true -> <<"yes">>;
		<<>> -> Default;
		[] -> Default;
		_ -> Dato
	end.


bool_convert(Dato) ->
	case Dato of
		<<"f">> -> false;
		<<"t">> -> true;
		_ -> false
	end.


list_convert(Dato) ->
        if erlang:is_list(Dato) ->
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

atom_convert(Dato)->
        if erlang:is_atom(Dato) ->
                Dato;
        erlang:is_list(Dato) ->
                erlang:list_to_atom(Dato);
        erlang:is_integer(Dato) ->
                erlang:list_to_atom(erlang:integer_to_list(Dato));
        erlang:is_binary(Dato) ->
                erlang:list_to_atom(erlang:binary_to_list(Dato));
        true ->
                ""
        end.

integer_convert_d(Dato,Default) ->
        if Dato =:= [] ->
                Default;
        erlang:is_integer(Dato) ->
                Dato;
        erlang:is_list(Dato) ->
                erlang:list_to_integer(Dato);
        erlang:is_binary(Dato) ->
                integer_convert(list_convert(Dato));
        true ->
                Default
        end.

integer_convert(Dato) ->
        if Dato =:= [] ->
                0;
        erlang:is_integer(Dato) ->
                Dato;
        erlang:is_list(Dato)  ->
                erlang:list_to_integer(Dato);
        erlang:is_binary(Dato) ->
                integer_convert(list_convert(Dato));
        true ->
                0
        end.

binary_convert(Dato) ->
	if erlang:is_binary(Dato) ->
		Dato;
	true ->
		if erlang:is_list(Dato) ->
			erlang:list_to_binary(Dato);
		true ->
			binary_convert(list_convert(Dato))
		end
	end.









%% @spec  connect_asterisk_node(Node)  -> {ok, NodeName} | {error, NodeName}
%% @doc Realiza las conecciones al nodo de asterisk
%% @end

%% TODO: Crear un monitor para los nodos.  Así poder realizar 
%% cambios en tiempo real de las llamadas.

connect_asterisk_node(Node) when is_list(Node) ->
	connect_asterisk_node(functions:atom_convert(Node));

connect_asterisk_node(Node) when is_atom(Node)->
	%%FIXME: Esto no se debe de hacer. De todos modos, cuando el erlast se reinicia
	%% debería de conectar con el acd.
	net_kernel:connect_node(Node),
	case net_kernel:connect_node(Node) of
		true ->  {ok, Node};
		false ->  {error, Node};
		_ -> {error, Node}
	end.

