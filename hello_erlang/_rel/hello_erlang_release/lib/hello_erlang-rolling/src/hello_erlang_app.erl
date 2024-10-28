-module(hello_erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}, {"/info", info_handler, []}]}
        
    ]),
    {ok, _} = cowboy:start_tls(my_http_listener,
        [{port, 8080}, {certfile, "C:/Users/mustafa/Desktop/cowboy/certfile.pem"}, {keyfile, "C:/Users/mustafa/Desktop/cowboy/keyfile.pem"}],
        #{env => #{dispatch => Dispatch}}
    ),
    hello_erlang_sup:start_link().

stop(_State) ->
	ok.
