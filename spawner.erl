-module(spawner).

-export([start/0 , process/1 , find_max/0 , find_min/0 , find_avg/0]).


%% # of processes
-define(LIMIT , 5).

%% Timeout of receive 
-define(TIMEOUT , 2*1000).

%% Interval for each Gossip Iteration
-define(INTERVAL , 3*1000).


listen() ->

       {_ , Me} = process_info( self() , registered_name ),

       receive

		{"Your Successor", Successor} ->
		       io:format(" 	      [~p] -> [~p]~n",[Me , Successor]),
		       listen();

		{"Your Predecessor", Predecessor} ->
		       io:format("[~p] -> [~p]~n",[Predecessor, Me]),
		       listen();

		wake_up ->
			io:format("[~p] will do gossip~n", [Me]),
			erlang:send_after(?INTERVAL , Me , wake_up),
			listen();

		find_max ->
			io:format("Ok, i will start finding max"),
			erlang:send_after(?INTERVAL , Me , wake_up),
			listen();

		find_min ->
			io:format("Ok, i will start finding min"),
			erlang:send_after(?INTERVAL , Me , wake_up),
			listen();

		find_avg ->
			io:format("Ok, i will start finding avg"),
			erlang:send_after(?INTERVAL , Me , wake_up),
			listen()

	after ?INTERVAL ->
	      listen()

	end.


process(NeighborList) ->

	{_, Me} = process_info( self() , registered_name ),
	[Predecessor, Successor] = NeighborList,

	%%Predecessor ! {"Your Successor", Me},
	%%Successor ! {"Your Predecessor", Me},

	%%erlang:send_after(?INTERVAL , Me , wake_up),
	listen().


getRingNeighborList(MyNumber) ->
	Predecessor = list_to_atom( string:concat( "process" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "process" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	NeighborList = [Predecessor , Successor],
	NeighborList.


do_spawn(0) ->
	ok;

do_spawn(N) ->
	ProcessName = list_to_atom( string:concat( "process" , integer_to_list( ?LIMIT - N ) ) ),
	register(ProcessName , spawn(?MODULE , process , [getRingNeighborList( ?LIMIT - N )])),
	do_spawn(N-1).


start() ->
	do_spawn(?LIMIT).


find_max() ->
	process1 ! find_max.


find_min() ->
	process1 ! find_min.


find_avg() ->
	process1 ! find_avg.
