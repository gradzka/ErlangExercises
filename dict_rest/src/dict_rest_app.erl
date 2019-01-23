-module(dict_rest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/list", dict_rest_handler, [list]},
               {"/get/:word", dict_rest_handler, [get]},
               {"/create/:word", dict_rest_handler, [create]},
               {"/update/:word", dict_rest_handler, [update]},
               {"/delete/:word", dict_rest_handler, [delete]},
               {"/help", dict_rest_handler, [help]},
               {"/", dict_rest_handler, [help]}
              ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),	
	dict_rest_sup:start_link().

stop(_State) ->
	ok.
