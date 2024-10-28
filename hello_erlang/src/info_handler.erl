-module(info_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"info? meh you don't need it">>,
        Req0),
    {ok, Req, State}.
