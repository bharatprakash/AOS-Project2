-module(tmp).

-export([node/2, start_the_game/1, listen/0, find_max/0,fetch_response_find_max/0, start/0, find_average/0]).

-define(LIMIT , 1000).

node(MyNum, NeighbourList) ->
	
	put(myNumber, (MyNum)),
	put(myMaxNumber, (MyNum)),
	put(myAveNumber, (MyNum)),
	put(myNeighbourList, (NeighbourList)),
	io:format("I am process ~w Neighbours ~w  MAX: ~w ~n", [get('myNumber'), NeighbourList, get('myMaxNumber')]),
	listen().

listen() ->

	receive
	
	{"MAX", Num, PID} ->
			PID ! {"MAX_Reply", get('myMaxNumber'), self()},
			io:format("~w Received Request from ~w  , MAX ~w ~n", [self(), PID, get('myMaxNumber')]),
			MaxNum = get('myMaxNumber'),
			if 
				MaxNum < Num ->
					put('myMaxNumber',(Num));
				true  ->
					io:format("")
			end,
			push_find_max();
	
	{"AVERAGE", Num, PID} ->
			PID ! {"AVERAGE_Reply", get('myAveNumber'), self()},				
			io:format("~w Received Request from ~w  , AVE ~w ~n", [self(), PID, get('myAveNumber')]),
			Avg = get('myAveNumber'),
			put('myAveNumber',((Avg + Num)/2)), 
			push_find_average();

	find_max ->
			io:format("Ok, i will start finding max ~w ~n",[self()]),
			push_find_max(),
			listen();

	find_average ->
			io:format("Ok, i will start finding average ~w ~n",[self()]),
			push_find_average(),
			listen()

			
			
	after 1000 ->
		listen()
		
	end.


push_find_max() ->
		io:format("Sending Request from ~w  , MAX ~w ~n", [self(), get('myMaxNumber')]),
		random:seed(now()),
		lists:nth(random:uniform(length(get('myNeighbourList'))),get('myNeighbourList')) ! {"MAX", get('myMaxNumber'), self()},	
		fetch_response_find_max().

fetch_response_find_max() ->
	receive
	
		{"MAX_Reply", Num, PID} ->
			io:format("~w Received MAX Reply from ~w  , MAX ~w ~n", [self(), PID, get('myMaxNumber')]),
			MaxNum = get('myMaxNumber'),
			if 
				MaxNum < Num ->
					put('myMaxNumber',(Num));
				true ->
					io:format("")
			end,
			listen()
		
		after 3000 ->
		listen()


	end.


push_find_average() ->
		io:format("Sending Request from ~w  , AVE~w ~n", [self(), get('myAveNumber')]),
		random:seed(now()),
		lists:nth(random:uniform(length(get('myNeighbourList'))),get('myNeighbourList')) ! {"AVERAGE", get('myAveNumber'), self()},	
		fetch_response_find_average().

fetch_response_find_average() ->
	receive
	
		{"AVERAGE_Reply", Num, PID} ->
			io:format("~w Received AVE Reply from ~w  , AVE ~w ~n", [self(), PID, get('myAveNumber')]),
			Avg = get('myAveNumber'),
			put('myAveNumber',((Avg + Num)/2)), 
			listen()
		
		after 3000 ->
		listen()

	end.



start_the_game(0) ->
	io:format("The game has started ~n", []);	

start_the_game(NumNodes) ->
	create_node(NumNodes-1),
	start_the_game(NumNodes-1).	

start() ->
	start_the_game(?LIMIT).	


create_node(Num) ->
	register(list_to_atom(string:concat("process",integer_to_list(Num))), spawn(tmp, node, [Num, getRingNeighborList(Num)])).	



getRingNeighborList(MyNumber) ->
	Predecessor = list_to_atom( string:concat( "process" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "process" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	NeighborList = [Predecessor , Successor],
	NeighborList.

find_max() ->
	process1 ! find_max.

find_average() ->
	process1 ! find_average.
