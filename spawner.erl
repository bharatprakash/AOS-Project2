
-module(spawner).

-export([start/0 , process/2 , findMax/0 , calculateMax/0 , findMin/0 , calculateMin/0 , findAvg/0 , calculateAvg/0]).


%% # of processes
-define(LIMIT , 10).

%% # of Fragments
-define(FLIMIT , 5).

%% Timeout of receive 
-define(TIMEOUT , 1000).

%% Interval for each Gossip Iteration
-define(INTERVAL , 100).


findAvg(X , Y) ->
	Avg = (X + Y) / 2,
	Avg.


findMin(X , Y) ->
	if
		X > Y ->
		        Min = Y;
		true ->
			Min = X
	end,
	Min.


findMax(X , Y) ->
	if
		X > Y ->
		        Max = X;
		true ->
			Max = Y
	end,
	Max.


getNeighbor() ->
	MyNeighbors = get('neighborList'),
	random:seed(now()),
	Neighbor = lists:nth(random:uniform(length(MyNeighbors)) , MyNeighbors),
	IsPresent = lists:member(Neighbor , registered()),
	if
		IsPresent == (false) ->
			  SelectedNeighbor = getNeighbor();
		true ->
			  SelectedNeighbor = Neighbor
	end,
	SelectedNeighbor.


selectNeighbor() ->
	%%Neighbor = get('name'),
	getNeighbor().


%% change according to the secrets
update(NeighborSecret) ->
	MySecret = get('secret'),
	HisSecret = lists:nth(1 , NeighborSecret),
	{_ , HisValue} = HisSecret,
	
	if
		length(MySecret) == (0) ->
			MyNumber = get('number'),
			%%Max = findMax(HisValue,MyNumber);
			Avg = findAvg(HisValue,MyNumber);
			%%Min = findMin(HisValue,MyNumber);
		true ->
			MySingleSecret = lists:nth(1 , MySecret),
			{_ , MyValue} = MySingleSecret,
			%%Max = findMax(HisValue,MyValue)
			Avg = findAvg(HisValue,MyValue)
			%%Min = findMin(HisValue,MyValue)
	end,

	%%MyNewSecret = [{max , Max}],
	MyNewSecret = [{avg , Avg}],
	%%MyNewSecret = [{min , Min}],
	Me = get('number'),
	if
		(Me rem 10) == (0) ->
		     	 %%io:format("Result | ~p | | ~p |~n",[get('name') , Max]);
		     	 io:format("Result | ~p | | ~p |~n",[get('name') , Avg]);
		     	 %%io:format("Result | ~p | | ~p |~n",[get('name') , Min]);
		true ->
			 io:format("")
	end,
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
	Name = get('name'),
	if
		Name /= Neighbor ->
			Secret = get('secret'),
			Neighbor ! {push_request , Secret , Name},
			waitPushResponse();
		true ->
			io:format("")
	end,
	io:format("").


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
			buildSecret(NeighborSecret , length(NeighborSecret)),
			whereis(Neighbor) ! {pull_complete , get('secret')},
			update(NeighborSecret);
			%%io:format("pull response from | ~p | | ~p | | ~p |~n",[Neighbor , registered() , whereis(Neighbor)]);

		 no_secret ->
		 	%%io:format("| ~p | no secret~n",[get('name')])
			io:format("")

	after ?TIMEOUT ->
		io:format("")

	end.


pull() ->
	Neighbor = selectNeighbor(),
	Name = get('name'),
	if
		Neighbor /= Name ->
			 whereis(Neighbor) ! {pull_request , Name},
			 waitPullResponse();
		true ->
			 io:format("")
	end,
	io:format("").


pushpull() ->
	Phase = get('phase'),
	%%io:format("| ~p | Phase | ~p |~n",[get('name'),get('phase')]),
	if
		Phase == (push) ->
		      	%%io:format("| ~p | in push~n",[get('name')]),
			Secret = get('secret'),
			if
				length(Secret) /= (0) ->
				push();
			true ->
				io:format("")
			end,
			put(phase , (pull));
		true ->
		      	%%io:format("| ~p | in pull~n",[get('name')]),
			pull(),
			put(phase , (push))
	end,
	%%io:format("| ~p | return from pull~n",[get('name')]).
	io:format("").


buildSecret(NeighborSecret , 0) ->
	io:format("");
buildSecret(NeighborSecret , I) ->
	{Secret , _} = lists:nth(I , NeighborSecret),
	IsSecret = lists:keysearch(Secret , 1 , get('secret')),
	if
		IsSecret == (false) ->
			NewSecret = [{max,get('number')} | get('secret')],
			put(secret , (NewSecret));
		true ->
			io:format("")
	end,
	buildSecret(NeighborSecret , I-1).


listen() ->

	pushpull(),
	%%io:format("| ~p | return from pushpull~n",[get('name')]),

	receive

		{pull_request , Neighbor} ->
			%%io:format("| ~p | : pull request from | ~p | | ~p | ~p |~n",[get('name') , Neighbor , whereis(Neighbor) , registered()]),
			Secret = get('secret'),
			if
				length(Secret) == (0) ->
				       	whereis(Neighbor) ! no_secret,
					io:format("");
				true ->
					whereis(Neighbor) ! {pull_response , get('secret') , get('name')},
					waitPullComplete()
			end,
			listen();

		{push_request , NeighborSecret , Neighbor} ->
			buildSecret(NeighborSecret , length(NeighborSecret)),
			Neighbor ! {push_response , get('secret')},
			update(NeighborSecret),
			listen();

		find_avg ->
			NewSecret = [{avg,get('number')} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		find_max ->
			NewSecret = [{max,get('number')} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		find_min ->
			NewSecret = [{min,get('number')} | get('secret')],
			put(secret , (NewSecret)),
			listen()

	after ?TIMEOUT ->
	      listen()

	end.


init_dict(MyNumber, NeighborList) ->
	put(number , (MyNumber)),
	%%{_ , Me} = process_info( self() , registered_name ),
	Me = list_to_atom( string:concat( "p" , integer_to_list(MyNumber) ) ),
	put(name , (Me)),
	put(neighborList , (NeighborList)),
	put(secret , ([])),
	put(phase , (push)),
	io:format("| ~p | | ~p | | ~p | | ~p | | ~p |~n",[get('number') , get('name') , get('neighborList') , get('secret') , self()]),
	io:format("").


process(MyNumber , NeighborList) ->

	init_dict(MyNumber , NeighborList),
	%%erlang:trace(self(), true, ['receive', {tracer, Creator}]),
	%%erlang:send_after(?INTERVAL , Me , wake_up),
	listen(),
	io:format("| ~p | I am exiting",[get('name')]).


getRingNeighborList(MyNumber) ->
	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	NeighborList = [Me , Predecessor , Successor],
	NeighborList.


floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.


log2(X) ->
	math:log(X) / math:log(2).


getList(MyNumber , List , 0) ->
	List;
getList(MyNumber , List , I) ->
	NewList = [list_to_atom( string:concat( "p" , integer_to_list(trunc(MyNumber + math:pow(2,I-1))))) | List],
	getList(MyNumber , NewList , I-1).

getChordNeighborList(MyNumber) ->
	Length = floor(log2(?LIMIT)),
	%%io:format("Length = ~p~n",[Length]),
	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	T1 = [Me | getList(MyNumber , [] , Length)],
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	IsSuccessor = lists:member(Successor , T1),
	if
		IsSuccessor == (false) ->
			T2 = [Successor | T1];
		true ->
			T2 = T1
	end,
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	IsPredecessor = lists:member(Predecessor , T2),
	if
		IsPredecessor == (false) ->
			T3 = [Successor | T2];
		true ->
			T3 = T2
	end,
	MirrorMyNumber = (?LIMIT - MyNumber - 1),
	MirrorMe = list_to_atom( string:concat( "p" , integer_to_list( MirrorMyNumber ))),
	IsMirrorMe = lists:member(MirrorMe , T3),
	if
		IsMirrorMe == (false) ->
			NeighborList = [MirrorMe | T3];
		true ->
			NeighborList = T3
	end,

	NeighborList.


getMirrorRingNeighborList(MyNumber) ->
	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	T1 = [Me , Predecessor , Successor],
	MirrorMyNumber = (?LIMIT - MyNumber - 1),
	MirrorMe = list_to_atom( string:concat( "p" , integer_to_list( MirrorMyNumber ))),
	IsMirrorMe = lists:member(MirrorMe , T1),
	if
		IsMirrorMe == (false) ->
			NeighborList = [MirrorMe | T1];
		true ->
			NeighborList = T1
	end,
	%%MirrorSuccessor = list_to_atom( string:concat( "p" , integer_to_list( ((MirrorMyNumber + 1) rem ?LIMIT )))),
	%%IsMirrorSuccessor = lists:member(MirrorSuccessor , T2),
	%%if
	%%	IsMirrorSuccessor == (false) ->
	%%		T3 = [MirrorSuccessor | T2];
	%%	true ->
	%%		T3 = T2
	%%end,
	%%MirrorPredecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MirrorMyNumber - 1) rem ?LIMIT )))),
	%%IsMirrorPredecessor = lists:member(MirrorPredecessor , T3),
	%%if
	%%	IsMirrorPredecessor == (false) ->
	%%		NeighborList = [MirrorPredecessor | T3];
	%%	true ->
	%%		NeighborList = T3
	%%end,
	%%io:format("MN | ~p |  MMN | ~p | MS | ~p | IMS | ~p | MP | ~p | IMP | ~p | N | ~p |~n",[MyNumber , MirrorMyNumber , MirrorSuccessor , IsMirrorSuccessor , MirrorPredecessor , IsMirrorPredecessor , NeighborList]),
	NeighborList.


do_spawn(0) ->
	ok;

do_spawn(N) ->
    	ProcessName = list_to_atom( string:concat( "p" , integer_to_list( ?LIMIT - N ) ) ),
	process_flag(trap_exit, true),
	%%register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getRingNeighborList( ?LIMIT - N )])),
	register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getChordNeighborList( ?LIMIT - N )])),
	%%register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getMirrorRingNeighborList( ?LIMIT - N )])),
	%%timer:sleep(2),
	do_spawn(N-1).


loop() ->
    receive

        {Exit , PID} -> 
	      {k, F} = file:open("exit.txt", [read, write]),
	      io:write({F, donnie, "Donnie Pinkston"}),
	      file:close(F),
	      io:format("@@@@@@@@@ ~p @@@@@@@@ | ~p |~n", [PID , Exit]);
	{Exit , PID , normal} -> 
	      io:format("@@@@@@@@@ ~p @@@@@@@@ | ~p |~n", [PID , Exit]),
	      {k, F} = file:open("exit.txt", [read, write]),
	      io:format("~p.~n",[{F, donnie, "Donnie Pinkston"}]),
	      file:close(F),
	      exit(normal);
        {Exit , PID , Reason} -> 
	      {k, F} = file:open("exit.txt", [read, write]),
	      io:write({F, donnie, "Donnie Pinkston"}),
	      file:close(F),
	      io:format("@@@@@@@@@ ~p @@@@@@@@ | ~p | | ~p |~n", [PID , Exit , Reason])
    end,
    loop().

build(0 , List) ->
	List;
build(N , List) ->
     	 ProcessName = list_to_atom( string:concat( "p" , integer_to_list( N ) ) ),
	 IsPresent = lists:member(ProcessName , registered()),
	 if
		IsPresent == (true) ->
			  NewList = build(N-1 , List);
		true ->
			  NewList = [ProcessName | build(N-1 , List)]
	 end,
	 %%build(N-1 , NewList),
	 NewList.

check() ->
	io:format("Dead | ~p |~n", [build(?LIMIT-1 , [])]),
	timer:sleep(1000),
	check().

start() ->
	do_spawn(?LIMIT),
	%%timer:sleep(1000),
	%%findMax(),
	findAvg(),
	%%findMin(),
	%%loop().
	check(),
	io:format("").


calculateMax() ->
	whereis(p0) ! find_max,
	exit(self() , "end of purpose").

findMax() ->
	spawn(?MODULE , calculateMax , []).


calculateMin() ->
	whereis(p0) ! find_min,
	exit(self() , "end of purpose").

findMin() ->
	spawn(?MODULE , calculateMin , []).


calculateAvg() ->
	whereis(p0) ! find_avg,
	exit(self() , "end of purpose").

findAvg() ->
	spawn(?MODULE , calculateAvg , []).
