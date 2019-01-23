-module(math_server_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {QueryBin, Req1} = cowboy_req:qs(Req),
	QueryStr = binary_to_list(QueryBin),
	QueryTokens = string:tokens(QueryStr, "&"),
	Op = hd(QueryTokens),
	Args = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, tl(QueryTokens)),
	Reply = if 
		Op == "plus" ->
			Res = plus(Args),
			integer_to_list(Res);
		Op == "minus" ->
			Res = minus(Args),
			integer_to_list(Res);
		Op == "mult" ->
			Res = mult(Args),
			integer_to_list(Res);
		Op == "div" ->
			case lists:member(0, Args) of
				true ->
					"Error, don't divide by 0";
				false ->
					Res = divi(Args),
					float_to_list(Res)
			end;		
		true ->
			"Error, operator undefined"
	end,
	ReplyBin = list_to_binary(Reply),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        [ReplyBin],
        Req1),
    {ok, Req2, State}.

plus([]) ->
	0;
plus([H|T]) ->
	H + plus(T).

minus([]) ->
	0;
minus([H|T]) ->
	H - plus(T).	

mult([]) ->
	1;
mult([H|T]) ->
	H * mult(T).
		
divi([]) ->
	1;
divi([H|T]) ->
	H / mult(T).
	
terminate(_Reason, _Req, _State) ->
	ok.
