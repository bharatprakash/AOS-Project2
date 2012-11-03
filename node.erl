-module(node).
-export([start/1, start/2, new_node/1, new_node/2]).

listen() ->
	 receive
		{PID, Message} ->
		      io:format("~p from ~p~n",[Message, PID]),
		      PID ! accepted,
		      listen();
		accepted ->
			 io:format("My request accepted~n"),
			 listen()
	end.

new_node(Name) ->
	io:format("New node of name : ~p~n", [Name]),
	listen().

new_node(Name, JoinTo) ->
	io:format("New node of name : ~p~n", [Name]),
	io:format("Wants to join : ~p~n", [JoinTo]),
	{node,JoinTo} ! {self(),"I want to join you"},
	listen().

start(Name) ->
	register(node, spawn(node, new_node,[Name])).

start(Name, JoinTo) ->
	register(node, spawn(node, new_node,[Name, JoinTo])).
