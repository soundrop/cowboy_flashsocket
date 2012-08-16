-module(cowboy_flashsocket_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]).
-export([init/4]).


-include("flashsocket.hrl").

-record(state, {
	listener :: pid(),
	connection :: #flashsocket_connection{},
	handler :: module(),
	opts :: any(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	buffer = <<>> :: binary()
}).

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
	Connection = #flashsocket_connection{
		socket = Socket,
		transport = Transport
	},
	Handler = proplists:get_value(handler, Opts),
	ok = cowboy:accept_ack(ListenerPid),
	handler_init(#state{
		listener = ListenerPid,
		connection = Connection,
		handler = Handler,
		opts = Opts
	}).

%% @private
-spec terminate(#state{}) -> ok.
terminate(State) ->
	Connection = State#state.connection,
	Transport = Connection#flashsocket_connection.transport,
	Socket = Connection#flashsocket_connection.socket,
	Transport:close(Socket),
	ok.

%% @private
-spec handler_init(#state{}) -> closed.
handler_init(State) ->
	Handler = State#state.handler,
	Connection = State#state.connection,
	Opts = State#state.opts,
	Transport = Connection#flashsocket_connection.transport,
	try Handler:flashsocket_init(Transport:name(), Connection, Opts) of
		{ok, HandlerState} ->
			handler_before_loop(State#state{messages = Transport:messages()}, HandlerState);
		shutdown ->
			terminate(State)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in flashsocket_init/3~n"
			"   for the reason ~p:~p~n** Options were ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, erlang:get_stacktrace()])
	end.

%% @private
-spec handler_call(#state{}, any(), atom(), any()) -> closed.
handler_call(State, HandlerState, Callback, Message) ->
	Handler = State#state.handler,
	try Handler:Callback(Message, State#state.connection, HandlerState) of
		{ok, HandlerState2} ->
			parse_request(State, HandlerState2);
		{reply, Payload, HandlerState2} ->
			case flashsocket_send(Payload, State) of
				{error, Reason} ->
					handler_terminate(State, HandlerState2, {error, Reason});
				_ ->
					parse_request(State, HandlerState2)
			end;
		{shutdown, HandlerState2} ->
			flashsocket_close(State, HandlerState2, {normal, shutdown})
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in ~p/3~n"
			"   for the reason ~p:~p~n** Message was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Callback, Class, Reason, Message, State#state.opts,
			 HandlerState, erlang:get_stacktrace()]),
		flashsocket_close(State, HandlerState, {error, handler})
	end.

%% @private
-spec handler_terminate(#state{}, any(), atom() | {atom(), atom()}) -> closed.
handler_terminate(State, HandlerState, TerminateReason) ->
	Handler = State#state.handler,
	Connection = State#state.connection,
	try
		Handler:flashsocket_terminate(TerminateReason, Connection, HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in flashsocket_terminate/3~n"
			"   for the reason ~p:~p~n** Initial reason was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, TerminateReason, State#state.opts,
			 HandlerState, erlang:get_stacktrace()])
	end,
	terminate(State),
	closed.

%% @private
-spec handler_before_loop(#state{}, any()) -> closed.
handler_before_loop(State, HandlerState) ->
	Connection = State#state.connection,
	Transport = Connection#flashsocket_connection.transport,
	Socket = Connection#flashsocket_connection.socket,
	Transport:setopts(Socket, [{active, once}]),
	handler_loop(State, HandlerState).

%% @private
-spec handler_loop(#state{}, any()) -> closed.
handler_loop(State = #state{messages = {OK, Closed, Error}}, HandlerState) ->
	Connection = State#state.connection,
	Socket = Connection#flashsocket_connection.socket,
	receive
		{OK, Socket, Data} ->
			Buffer = State#state.buffer,
			parse_request(State#state{buffer = <<Buffer/binary, Data/binary>>}, HandlerState);
		{Closed, Socket} ->
			handler_terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, HandlerState, {error, Reason});
		Message ->
			handler_call(State, HandlerState, flashsocket_info, Message)
	end.

%% @private
-spec parse_request(#state{}, any()) -> ok.
parse_request(State, HandlerState) ->
	case z_split(State#state.buffer) of
		{Payload, Rest} ->
			handler_call(State#state{buffer = Rest}, HandlerState, flashsocket_handle, {text, Payload});
		_ ->
			handler_before_loop(State, HandlerState)
	end.

%% @private
-spec flashsocket_close(#state{}, any(), {atom(), atom()}) -> closed.
flashsocket_close(State, HandlerState, Reason) ->
	handler_terminate(State, HandlerState, Reason).

%% @private
flashsocket_send({text, Payload}, State) ->
	Connection = State#state.connection,
	Transport = Connection#flashsocket_connection.transport,
	Socket = Connection#flashsocket_connection.socket,
	Transport:send(Socket, [Payload, 0]).

%% @private
z_split(B) when is_binary(B) ->
	z_split3(B, 0, size(B)).

%% @private
z_split3(B, N, X) ->
	case B of
		<<_:N/binary, 0, _/binary>> ->
			<<B1:N/binary, 0, B2/binary>> = B,
			{B1, B2};
		_ when X > N ->
			z_split3(B, N + 1, X);
		_ ->
			B
	end.


