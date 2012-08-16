
-record(flashsocket_connection, {
	socket    :: inet:socket(),
	transport :: module()
}).

