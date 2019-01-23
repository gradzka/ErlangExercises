-module(ws_calc_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([find/2, qs_transl/1, kv_lst/1, 
		 process_query/2, reply/2]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.
	
handle(_Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.  	

websocket_init(_, Req, _Opts) ->
	{ok, Req, 0}.

websocket_handle({text, Data}, Req, State) ->
	try qs_transl(binary_to_list(Data)) of
		Query ->
			try process_query(Query, State) of		
				{Reply, NewState} ->
					{reply, {text, << Reply/binary >>}, Req, NewState, hibernate}
			catch
				error:_ ->
					{reply, {text, << "Error" >>}, Req, State, hibernate}
			end
	catch
		error:_ ->
			{reply, {text, << "Error" >>}, Req, State, hibernate}
	end;
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State, hibernate};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
	
terminate(_Reason, _Req, _State) -> 
    ok.	

process_query(Query, State) -> 
    case list_to_atom(find(op,Query)) of	
	   set -> 
	      ArgStr = list_to_atom(find(arg,Query)),
	      case ArgStr of
		     'NaN' -> 
			    { reply('NaN',[]),
				  'NaN' };
			 'Infinity' -> 
				{ reply('Infinity',[]),
				  'Infinity' };
			 _ ->
			    Arg = arg_to_num(find(arg,Query)),
		        { reply("~w",[Arg]),
			      Arg }
		  end; 
	   plus -> 
		  case State of
		     'NaN' -> 
			    Arg = arg_to_num(find(arg,Query)),
			    { reply("~w",[Arg]),
				  Arg };
			 'Infinity' -> 
				{ reply('Infinity',[]),
				  'Infinity' };
			 _ ->
			    Arg = arg_to_num(find(arg,Query)),
				Result = State + Arg,
				{ reply("~w",[Result]),
				  Result }
		  end;   
	   minus ->
	      case State of
		     'NaN' -> 
			    Arg = arg_to_num(find(arg,Query)),
			    { reply("~w",[Arg]),
				  Arg };
			 'Infinity' -> 
				{ reply('Infinity',[]),
				  'Infinity' };
			 _ ->
			    Arg = arg_to_num(find(arg,Query)),
				Result = State - Arg,
				{ reply("~w",[Result]),
				  Result }
		  end; 
	   mult ->
	      case State of
		     'NaN' -> 
			    { reply("~w",[0]),
				  0 };
			 'Infinity' -> 
			    Arg = arg_to_num(find(arg,Query)),
			    case Arg of
		           0 -> 
			          { reply('NaN',[]),
				        'NaN' };
			       0.0 -> 
				      { reply('NaN',[]),
				        'NaN' };
			       _ ->
			          { reply('Infinity',[]),
				        'Infinity' }
		        end;
			 _ ->
			    Arg = arg_to_num(find(arg,Query)),
				Result = State * Arg,
				{ reply("~w",[Result]),
				  Result }
		  end; 
	   divi ->
	      case State of
		     'NaN' -> 
			    Arg = arg_to_num(find(arg,Query)),
			    case Arg of
		           0 -> 
			          { reply('NaN',[]),
				        'NaN' };
			       0.0 -> 
				      { reply('NaN',[]),
				        'NaN' };
			       _ ->
			          { reply("~w",[0]),
				        0 }
		        end;
			 'Infinity' -> 
			       { reply('Infinity',[]),
				     'Infinity' };
			 _ ->
			    Arg = arg_to_num(find(arg,Query)),
				case Arg of
				   0 -> 
				      { reply('NaN',[]),
				        'NaN' };
				   0.0 -> 
				      { reply('NaN',[]),
				        'NaN' };
			       _ ->
			          Result = State / Arg,
				      { reply("~w",[Result]),
				        Result }
		        end
		  end; 
	   sqrt ->
	      case State of
		     'NaN' -> 
			    { reply('NaN',[]),
				  'NaN' };
			 'Infinity' -> 
				{ reply('Infinity',[]),
				  'Infinity' };
			 _ ->
			    case State =< 0 of
                   true -> 
				      { reply('NaN',[]),
				        'NaN' };
			       _ ->
			          Result = math:sqrt(State),
				      { reply("~w",[Result]),
				        Result }
		        end
		  end; 
	   inv ->
	      case State of
		     'NaN' -> 
			    { reply('NaN',[]),
				  'NaN' };
			 'Infinity' -> 
				{ reply("~w",[0]),
				  0 };
			 _ ->
			    case State of
		          0 -> 
			     	{ reply('Infinity',[]),
		     		  State };
                  0.0 -> 
	          		{ reply('Infinity',[]),
		          	  State };
		          _ ->
		     	    Result = 1 / State,
		     		{ reply("~w",[Result]),
			     	  Result }
		        end
		  end; 
	   eq ->
	      { reply("~w",[State]),
			State };
	   ac ->
		  { reply("~w",[0]),
			0 };
	   _ ->
          { reply("Undefined operation.",[]),
		    State }
    end.

qs_transl(String) ->
    kv_lst(string:tokens(String,"&")).

kv_lst([]) -> [];
kv_lst([Eq|Eqs]) ->
	[Ks,Vs] = string:tokens(Eq,"="),
	[{list_to_atom(Ks),Vs}|kv_lst(Eqs)].

find(_,[]) -> undefined;
find(Key,[{Key,Value}|_]) -> Value;
find(Key,[_|T]) -> find(Key,T).

arg_to_num(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

reply(Format, Args) ->
	list_to_binary(io_lib:format(Format, Args)).	
