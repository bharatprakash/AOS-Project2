-module(spawner).
-export([start/0, do_spawn/1, process/1]).

-define(LIMIT, 10).

start() ->
	do_spawn(?LIMIT).


do_spawn(0) ->
	ok;
do_spawn(N) ->
	ProcessName = list_to_atom( string:concat( "process" , integer_to_list( ?LIMIT - N ) ) ),
	register(ProcessName , spawn(?MODULE , process , [getRingNeighborList( ?LIMIT - N )])),
	do_spawn(N-1).


process(NeighborList) ->
	{_, Me} = process_info( self() , registered_name ),
	[Predecessor, Successor] = NeighborList,
	Predecessor ! {"Your Successor", Me},
	Successor ! {"Your Predecessor", Me},
	listen().


getRingNeighborList(MyNumber) ->
	Predecessor = list_to_atom( string:concat( "process" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "process" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	NeighborList = [Predecessor , Successor],
	NeighborList.


listen() ->

       {_ , Me} = process_info( self() , registered_name ),
       receive

		{"Your Successor", Successor} ->
		       io:format(" 	      [~p] -> [~p]~n",[Me , Successor]),
		       listen();

		{"Your Predecessor", Predecessor} ->
		       io:format("[~p] -> [~p]~n",[Predecessor, Me]),
		       listen()
	end.
