-module(ws_calc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, ws_calc, "index.html"}},
			{"/websocket", ws_calc_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, ws_calc, "static"}}						
		]}
	]),
	cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),	
	ws_calc_sup:start_link().

stop(_State) ->
	ok.
