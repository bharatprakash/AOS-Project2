-module(spawner).

-export([start/0 , process/2 , findMax/0 , calculateMax/0 , find_min/0 , find_avg/0]).


%% # of processes
-define(LIMIT , 10000).

%% Timeout of receive 
-define(TIMEOUT , 500).

%% Interval for each Gossip Iteration
-define(INTERVAL , 1000).


findMax(X , Y) ->
	if
		X > Y ->
		        Max = X;
		true ->
			Max = Y
	end,
	Max.

selectNeighbor() ->
	MyNeighbors = get('neighborList'),
	random:seed(now()),
	lists:nth(random:uniform(length(MyNeighbors)) , MyNeighbors).


%% change according to the secrets
update(NeighborSecret) ->
	MySecret = get('secret'),
	HisSecret = lists:last(NeighborSecret),
	{_ , HisValue} = HisSecret,
	
	if
		MySecret == (undefined) ->
			MyNumber = get('number'),
			Max = findMax(HisValue,MyNumber);
		true ->
			MySingleSecret = lists:nth(1 , MySecret),
			{_ , MyValue} = MySingleSecret,
			Max = findMax(HisValue,MyValue)
	end,

	MyNewSecret = [{max , Max}],	
	io:format("putting |~p|~n",[Max]),
	put(secret , (MyNewSecret)).
	

waitPushResponse() ->
	receive

		{push_response , NeighborSecret} ->
		        update(NeighborSecret)

	after ?TIMEOUT ->
	      	io:format("")
	end.


push() ->
	Neighbor = selectNeighbor(),
	Secret = get('secret'),
	Name = get('name'),
	Neighbor ! {push_request , Secret , Name},
	waitPushResponse().


waitPullComplete() ->
	receive

		{pull_complete , NeighborSecret} ->
			update(NeighborSecret)

	after ?TIMEOUT ->
	      	io:format("")

	end.


waitPullResponse() ->
	receive

		{pull_response , NeighborSecret , Neighbor} ->
			update(NeighborSecret),
			Neighbor ! {pull_complete , get('secret')};

		 no_secret ->
		 	io:format("no secret~n")

	after ?TIMEOUT ->
		io:format("")

	end.


pull() ->
	Neighbor = selectNeighbor(),
	Name = get('name'),
	io:format("pull | ~p | | ~p |~n",[Name,Neighbor]),
	%%Neighbor ! {pull_request , Name},
	io:format("before calling neighbor~n"),
	waitPullResponse().


pushpull() ->
       Secret = get('secret'),
       if
		Secret /= (undefined) ->
		       push();
		true ->
		     io:format("")
       end.
       %%pull().


listen() ->

       pushpull(),

       receive

		%%wake_up ->
		%%	io:format("[~p] will do gossip~n", [Me]),
		%%	erlang:send_after(?INTERVAL , Me , wake_up),
		%%	listen();

		{push_request , NeighborSecret , Neighbor} ->
			update(NeighborSecret),
			Neighbor ! {push_response , get('secret')},
			listen();

		{pull_request , Neighbor} ->
			io:format("| ~p | pull request from | ~p |~n",[get('name') , Neighbor]),
			Secret = get('secret'),
			if
				Secret ==(undefined) ->
				       	Neighbor ! no_secret;
				true ->
					Neighbor ! {pull_response , get('secret') , get('name')},
					waitPullComplete()
			end,
			listen();

		find_max ->
			Secret = get('secret'),
			if
				Secret == (undefined) ->
				       NewSecret = [{max,get('number')}];
				true ->
				       NewSecret = [{max,get('number')} | get('secret')]
			end,
			put(secret , (NewSecret)),
			listen();

		find_min ->
			Secret = get('secret'),
			if
				Secret == (undefined) ->
				       NewSecret = [{min,get('number')}];
				true ->
				       NewSecret = [{min,get('number')} | get('secret')]
			end,
			put(secret , (NewSecret)),
			io:format("Secret|~p|~n",[get('secret')]),
			%%lang:send_after(?INTERVAL , Me , wake_up),
			listen();

		find_avg ->
			io:format("Ok, i will start finding avg"),
			%%lang:send_after(?INTERVAL , Me , wake_up),
			listen();

		_Others ->
			io:format("")

	after ?INTERVAL*2 ->
	      listen()

	end.


init_dict(MyNumber, NeighborList) ->
	put(number , (MyNumber)),
	{_ , Me} = process_info( self() , registered_name ),
	put(name , (Me)),
	put(neighborList , (NeighborList)).

process(MyNumber , NeighborList) ->

	init_dict(MyNumber , NeighborList),

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
	register(ProcessName , spawn(?MODULE , process , [(?LIMIT - N) , getRingNeighborList( ?LIMIT - N )])),
	do_spawn(N-1).


start() ->
	%%try
		do_spawn(?LIMIT).
	%%catch
		%%io:format("")
	%%end.


calculateMax() ->
	process0 ! find_max,
	exit(self() , "end of purpose").

findMax() ->
	spawn(?MODULE , calculateMax , []).


find_min() ->
	process1 ! find_min.


find_avg() ->
	process1 ! find_avg.
