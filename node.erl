-module(node).
-export([join/0, join/1, new_node/0, new_node/1, listen/1]).

listen(Neighbor_List) ->
	io:format("My neighbor list ~p~n", [Neighbor_List]),
	receive
		{From, JoinRequest} ->
		       New_Neighbor_List = [From | Neighbor_List],
		       io:format("~p from ~p~n",[JoinRequest, From]),
		       {node, From} ! {node(), accepted, Neighbor_List},
		       listen(New_Neighbor_List);
		{From, accepted, Returned_Neighbor_List} ->
		       io:format("My request accepted by ~p with ~p~n", [From, Returned_Neighbor_List]),
		       New_Neighbor_List = [From | Returned_Neighbor_List],
		       Sender = lists:nth(1, New_Neighbor_List),
		       {node, Sender} ! thanks,
		       listen(New_Neighbor_List);
		thanks ->
		       io:format("He said thanks~n"),
		       listen(Neighbor_List)
	end.

new_node() ->
	io:format("New node~n", []),
	listen([]).

new_node(JoinTo) ->
	io:format("New node~n", []),
	io:format("Want to join : ~p~n", [JoinTo]),
	{node, JoinTo} ! {node(), "I want to join you"},
	listen([]).

join() ->
	register(node, spawn(node, new_node,[])).

join(JoinTo) ->
	register(node, spawn(node, new_node,[JoinTo])).
