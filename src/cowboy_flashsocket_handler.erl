-module(cowboy_flashsocket_handler).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
	-> undefined | [{flashsocket_handle, 3} | {flashsocket_info, 3}
		| {flashsocket_init, 3} | {flashsocket_terminate, 3}, ...].
behaviour_info(callbacks) ->
	[{flashsocket_init, 3}, {flashsocket_handle, 3},
	 {flashsocket_info, 3}, {flashsocket_terminate, 3}];
behaviour_info(_Other) ->
	undefined.
