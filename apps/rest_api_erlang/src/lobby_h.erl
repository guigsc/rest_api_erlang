-module(lobby_h).

-behaviour(cowboy_handler).

-export([
	init/2, 
	allowed_methods/2, 
	content_types_accepted/2,
	content_types_provided/2,
	handler/2,
	delete_resource/2,
	resource_exists/2
]). 

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handler}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handler}], Req, State}.

init(Req0, State) ->
	{cowboy_rest, Req0, State}.

resource_exists(Req0=#{method := <<"DELETE">>}, State) ->
	#{player_id := PlayerId} = cowboy_req:match_qs([{player_id, [], undefined}], Req0),
	case is_player_in_lobby(binary_to_integer(PlayerId)) of
		{ok, IsPlayerInLobby} ->
			{IsPlayerInLobby, Req0, State};
		{error, Message} ->
			Req = cowboy_req:set_resp_body(jsx:encode([Message]), Req0),
			{false, Req, State}
	end;
resource_exists(Req, State) ->
	{true, Req, State}.

handler(Req, State) ->
	case cowboy_req:method(Req) of
    <<"POST">> -> 
			post(Req, State);
    <<"GET">> -> 
      get(Req, State)
  end.

post(Req, State) ->
	{ok, JsonData, _} = cowboy_req:read_body(Req),
	Data = jsx:decode(JsonData, [{return_maps, false}]),
	case is_player_in_lobby(proplists:get_value(<<"player_id">>, Data)) of
		{ok, true} ->
			Req1 = cowboy_req:set_resp_body(jsx:encode([{<<"ErrorMessage">>, <<"Player already in lobby">>}]), Req),
			{false, Req1, State};
		{ok, false} ->
			erlcloud_ddb2:put_item(<<"lobby">>, Data),
			{true, Req, State};
		{error, Message} ->
			Req1 = cowboy_req:set_resp_body(jsx:encode([Message]), Req),
			{false, Req1, State}
	end.

get(Req0, State) ->
	{ok, Data} = erlcloud_ddb2:scan(<<"lobby">>),
	JsonData = jsx:encode(Data),
	{JsonData, Req0, State}.	

delete_resource(Req, State) -> 
	#{player_id := PlayerId} = cowboy_req:match_qs([{player_id, [], undefined}], Req),
	case erlcloud_ddb2:delete_item(<<"lobby">>, [{<<"player_id">>, binary_to_integer(PlayerId)}]) of
		{ok, _} -> 
			{true, Req, State};
		{error, Message} ->
			Req1 = cowboy_req:reply(400,
				#{<<"content-type">> => <<"application/json">>},
				jsx:encode([Message]),
				Req),
			{false, Req1, State}
	end.

is_player_in_lobby(PlayerId) -> 
	case erlcloud_ddb2:get_item(<<"lobby">>, [{<<"player_id">>, PlayerId}]) of 
		{ok, []} ->
			{ok, false};
		{ok, _} -> 
			{ok, true};
		{error, Message} ->
			{error, Message}
	end.