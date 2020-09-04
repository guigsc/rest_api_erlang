-module(rest_api_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{'_', [
            {"/lobby", lobby_h, []}
        ]
    }],
    
    Dispatch = cowboy_router:compile(Routes),

    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8002}], 
        #{env => #{dispatch => Dispatch}}
    ),

    rest_api_erlang_sup:start_link().

stop(_State) ->
    ok.


