
-module(spawner).

-compile([nowarn_unused_function , nowarn_unused_vars]).

-export([start/0 , process/2 , findMax/0 , calculateMax/0 , findMin/0 , calculateMin/0 , findAvg/0 , calculateAvg/0 , updateFragment/2 , calculateUpdate/2 , retrieveFragment/1 , calculateRetrieve/1 , collectDeadProcesses/0]).


%% ---------- Definitions ----------

%% # of processes
-define(LIMIT , 1000).

%% # of Fragments
-define(FRAGLIMIT , trunc(?LIMIT/2)).

%% Timeout of receive 
-define(TIMEOUT , 1000).

%% Interval for each Gossip Iteration
-define(INTERVAL , 100).


%% ---------- Selection of Neighbor ----------

selectNeighbor() ->
	MyNeighbors = get('neighborList'),
	random:seed(now()),
	Neighbor = lists:nth(random:uniform(length(MyNeighbors)) , MyNeighbors),
	IsPresent = lists:member(Neighbor , registered()),
	if
		IsPresent == (false) ->
			  SelectedNeighbor = selectNeighbor();
		true ->
			  SelectedNeighbor = Neighbor
	end,
	SelectedNeighbor.


%% ---------- List of Operations in Secret ----------

getOperationList([]) ->
	L = [],
	L;
getOperationList([Secret | RemainingSecretList]) ->
	Operation = element(1 , Secret),
	[Operation | getOperationList(RemainingSecretList)].


%% ---------- Find Max of Two ----------

findMax2(X , Y) ->
	if
		X > Y ->
		        Max = X;
		true ->
			Max = Y
	end,
	Max.


findMyMax(0 , L , Max) ->
	Max;

findMyMax(N , L , Max) ->
	Nth = lists:nth(N , L),
        findMyMax(N-1 , L , findMax2(Max , Nth)).


doMaxUpdate(Secret) ->

	{_ , HisMax} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			MyMax = findMyMax(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue'))),
			Max = findMax2(HisMax , MyMax),
			put(secret , ([{max , Max} | get('secret')]));
		true ->
			{_ , {_ , MyMax}} = lists:keysearch(max , 1 , get('secret')),
			Max = findMax2(HisMax , MyMax),
			put(secret , [{max , Max} | lists:keydelete(max , 1 , get('secret'))])
	end,

	io:format("Max | ~p | | ~p |~n",[get('name') , Max]).


%% ---------- Find Min of Two ----------

findMin2(X , Y) ->
	if
		X > Y ->
		        Min = Y;
		true ->
			Min = X
	end,
	Min.


findMyMin(0 , L , Min) ->
	Min;

findMyMin(N , L , Min) ->
	Nth = lists:nth(N , L),
        findMyMin(N-1 , L , findMin2(Min , Nth)).


doMinUpdate(Secret) ->

	{_ , HisMin} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			MyMin = findMyMin(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue'))),
			Min = findMin2(HisMin , MyMin),
			put(secret , ([{min , Min} | get('secret')]));
		true ->
			{_ , {_ , MyMin}} = lists:keysearch(min , 1 , get('secret')),
			Min = findMin2(HisMin , MyMin),
			put(secret , [{min , Min} | lists:keydelete(min , 1 , get('secret'))])
	end,

	io:format("Min | ~p | | ~p |~n",[get('name') , Min]).


%% ---------- Find Avg of Two ----------

findAvg2(X , Xlen , Y , Ylen) ->
	((X * Xlen) + (Y * Ylen)) / (Xlen + Ylen).


doAvgUpdate(Secret) ->

	{_ , HisAvg , HisLen} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			FragValue = get('fragmentValue'),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, FragValue)) / length(FragValue),
			MyLen = length(FragValue),
			Avg = findAvg2(HisAvg , HisLen , MyAvg , MyLen),
			put(secret , ([{avg , Avg , MyLen} | get('secret')]));
		true ->
			{_ , {_ , MyAvg , MyLen}} = lists:keysearch(avg , 1 , get('secret')),
			Avg = findAvg2(HisAvg , HisLen , MyAvg , MyLen),
			put(secret , [{avg , Avg , MyLen} | lists:keydelete(avg , 1 , get('secret'))])
	end,
	io:format("Avg | ~p | | ~p , ~p |~n",[get('name') , Avg , MyLen]).


%% ---------- Fragment Update ----------

doUpdateFragUpdate(Secret) ->

        case lists:member(Secret , get('secret')) of

	        false ->
			{_ , HisId , HisValue} = Secret,
			MyFragId = get('fragmentId'),			
	 		if
			        HisId == (MyFragId) ->
      	 	      		        put(fragmentValue , (HisValue));
				true ->
		      		     io:format("")
	 		end,
			put(secret , ([Secret | get('secret')]));
		true ->
			io:format("")
	end,
	io:format("UpF | ~p | | ~p , ~p |~n", [get('name') , get('fragmentId') , get('fragmentValue')]).


%% ---------- Fragment Retrieval ----------

doRetrieveFragUpdate(Secret) ->

	{_ , HisFragId , HisFragValue} = Secret,

	IsPresent = getMatch(Secret , get('secret')),

	case  IsPresent of

	        false ->
			MyFragId = get('fragmentId'),
			if
				MyFragId == (HisFragId) ->
					put(secret , ([{retrieve_frag , MyFragId , get('fragmentValue')} | get('secret')]));
				true ->
					put(secret , ([Secret | get('secret')]))
			end;
		_ ->
			if
				length(HisFragValue) /=0 ->
					put(secret , ([Secret | lists:filter(fun(X) -> X /= (IsPresent) end, get('secret'))]));
				true ->
					true
			end
	end,
	io:format("ReF | ~p | | ~p |~n",[get('name') , get('secret')]).


%% ---------- Update Driver ----------
update([]) ->
	true;

update([Secret | RemainingSecret]) ->
	
        Operation = element(1,Secret),

	case Operation of
	        max ->
		        doMaxUpdate(Secret);
		min ->
		        doMinUpdate(Secret);
		avg ->
		        doAvgUpdate(Secret);
		update_frag ->
		        doUpdateFragUpdate(Secret);
		retrieve_frag ->
		        doRetrieveFragUpdate(Secret);
		_ ->
		        io:format("")
	end,
		
	update(RemainingSecret).

		
%% ---------- Push ----------

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


%% ---------- Pull ----------

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
			%%build(NeighborSecret),
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


%% ---------- Push And Pull Alternation ----------

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


%% ---------- Build The Secret To Be Returned ----------

doMaxBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			MyMax = findMyMax(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue'))),
			put(secret , ([{max , MyMax} | get('secret')]));
		true ->
			true
	end.


doMinBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			MyMin = findMyMin(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue'))),
			put(secret , ([{min , MyMin} | get('secret')]));
		true ->
			true
	end.


doAvgBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

	        false ->
			FragValue = get('fragmentValue'),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, FragValue)) / length(FragValue),
			MyLen = length(FragValue),
			put(secret , ([{avg , MyAvg , MyLen} | get('secret')]));
		true ->
			true
	end.


getMatch(_, []) ->
        false;

getMatch(Secret , [MySecret | MyRemainingSecret]) ->

	{HisSec , HisFragId , _} = Secret,
	{MySec , MyFragId , _} = MySecret,

        case  ((HisSec == (MySec)) and (HisFragId == (MyFragId)))of
	        true ->
		        MySecret;
		false ->
		        getMatch(Secret , MyRemainingSecret)
	end.


doRetrieveFragBuild(Secret) ->

	{_ , HisFragId , HisFragValue} = Secret,

	IsPresent = getMatch(Secret , get('secret')),

	case  IsPresent of

	        false ->
			MyFragId = get('fragmentId'),
			if
				MyFragId == (HisFragId) ->
					put(secret , ([{retrieve_frag , MyFragId , get('fragmentValue')} | get('secret')]));
				true ->
					put(secret , ([Secret | get('secret')]))
			end;
		_ ->
			if
				length(HisFragValue) /=0 ->
					put(secret , ([Secret | lists:filter(fun(X) -> X /= (IsPresent) end, get('secret'))]));
				true ->
					true
			end
	end.


build([]) ->
	true;

build([Secret | RemainingSecret]) ->

        Operation = element(1,Secret),

	case Operation of
	        max ->
		        doMaxBuild(Secret);
		min ->
		        doMinBuild(Secret);
		avg ->
		        doAvgBuild(Secret);
		retrieve_frag ->
			doRetrieveFragBuild(Secret);
		_ ->
		        io:format("")
	end,
		
	build(RemainingSecret).


%% ---------- Listen And PushPull Alternation of Process ----------

listen() ->

	pushpull(),

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
			%%build(NeighborSecret),
			Neighbor ! {push_response , get('secret')},
			update(NeighborSecret),
			listen();

		find_max ->
			NewSecret = [{max , findMyMax(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue')))} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		find_min ->
			NewSecret = [{min , findMyMin(length(get('fragmentValue')) , get('fragmentValue') , lists:nth(1 , get('fragmentValue')))} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		find_avg ->
			FragValue = get('fragmentValue'),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, FragValue)) / length(FragValue),
			MyLen = length(FragValue),
			put(secret , ([{avg , MyAvg , MyLen} | get('secret')])),
			listen();

		{update_fragment , FragmentId , Value} ->
			MyFragmentId = get('fragmentId'),
			if
				FragmentId == (MyFragmentId) ->
					 put(fragmentValue , (Value));
				true ->
					 io:format("")
			end,
			put(secret , ([{update_frag , FragmentId , Value} | get('secret')])),
			listen();

		{retrieve_fragment , FragmentId} ->
			io:format("Received msg~n"),
			MyFragmentId = get('fragmentId'),
			if
				FragmentId == (MyFragmentId) ->
				        MyFragmentValue = get('fragmentValue'),
					put(secret , ([{retrieve_frag , FragmentId , MyFragmentValue} | get('secret')]));
				true ->
					put(secret , ([{retrieve_frag , FragmentId , []} | get('secret')]))					
			end,
			listen()

	after ?TIMEOUT ->
	      listen()

	end.


%% ---------- Initialize the Process Dictionary ----------

init_dict(MyNumber, NeighborList) ->
	put(number , (MyNumber)),
	Me = list_to_atom( string:concat( "p" , integer_to_list(MyNumber) ) ),
	put(name , (Me)),
	put(neighborList , (NeighborList)),
	put(secret , ([])),
	put(fragmentId , (MyNumber rem ?FRAGLIMIT)),
	%%put(fragmentValue , (MyNumber rem ?FLIMIT)),
	put(fragmentValue , [(MyNumber rem ?FRAGLIMIT) , ((MyNumber rem ?FRAGLIMIT) + ?FRAGLIMIT)]),
	put(phase , (push)),
	io:format("| ~p | | ~p | | ~p | | ~p | | ~p , ~p |~n",[get('number') , get('name') , get('neighborList') , get('secret') , get('fragmentId') , get(fragmentValue)]),
	io:format("").


%% ---------- Entry Point of Process ----------

process(MyNumber , NeighborList) ->

	init_dict(MyNumber , NeighborList),
	listen(),
	io:format("| ~p | I am exiting",[get('name')]).


%% ---------- Ring Topology ----------

getRingNeighborList(MyNumber) ->
	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MyNumber - 1) rem ?LIMIT )))),
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem ?LIMIT )))),
	NeighborList = [Me , Predecessor , Successor],
	NeighborList.


%% ---------- Chord Topology ----------

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.


ceiling(X) ->
        T = erlang:trunc(X),
	case (X - T) of
	        Neg when Neg < 0 -> T;
		Pos when Pos > 0 -> T + 1;
		_ -> T
	end.


log2(X) ->
	math:log(X) / math:log(2).


getList(MyNumber , List , 0) ->
	List;

getList(MyNumber , List , I) ->
	NewList = [list_to_atom( string:concat( "p" , integer_to_list( trunc((MyNumber + math:pow(2,I-1))) rem ?LIMIT ))) | List],
	getList(MyNumber , NewList , I-1).


getChordNeighborList(MyNumber) ->
	Length = ceiling(log2(?LIMIT)),

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
			T3 = [Predecessor | T2];
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


%% ---------- Mirror Ring Topology ----------

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
			T2 = [MirrorMe | T1];
		true ->
			T2 = T1
	end,

	MirrorSuccessor = list_to_atom( string:concat( "p" , integer_to_list( ((MirrorMyNumber + 1) rem ?LIMIT )))),
	IsMirrorSuccessor = lists:member(MirrorSuccessor , T2),
	if
		IsMirrorSuccessor == (false) ->
			T3 = [MirrorSuccessor | T2];
		true ->
			T3 = T2
	end,

	MirrorPredecessor = list_to_atom( string:concat( "p" , integer_to_list( ((?LIMIT + MirrorMyNumber - 1) rem ?LIMIT )))),
	IsMirrorPredecessor = lists:member(MirrorPredecessor , T3),
	if
		IsMirrorPredecessor == (false) ->
			NeighborList = [MirrorPredecessor | T3];
		true ->
			NeighborList = T3
	end,

	NeighborList.


%% ---------- Creation of Processes ----------

do_spawn(0) ->
	ok;

do_spawn(N) ->
    	ProcessName = list_to_atom( string:concat( "p" , integer_to_list( ?LIMIT - N ) ) ),
	process_flag(trap_exit, true),
	%%register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getRingNeighborList( ?LIMIT - N )])),
	register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getChordNeighborList( ?LIMIT - N )])),
	%%register(ProcessName , spawn_link(?MODULE , process , [(?LIMIT - N) , getMirrorRingNeighborList( ?LIMIT - N )])),
	do_spawn(N-1).


%% ---------- Listsing Dead Processes ----------

checkIfDead(0 , List) ->
	List;

checkIfDead(N , List) ->
     	 ProcessName = list_to_atom( string:concat( "p" , integer_to_list( N ) ) ),
	 IsPresent = lists:member(ProcessName , registered()),
	 if
		IsPresent == (true) ->
			  NewList = checkIfDead(N-1 , List);
		true ->
			  NewList = [ProcessName | checkIfDead(N-1 , List)]
	 end,
	 NewList.


collectDeadProcesses() ->
	io:format("Dead | ~p |~n", [checkIfDead(?LIMIT-1 , [])]),
	timer:sleep(3000),
	collectDeadProcesses().


%% ---------- Entry Point ----------

start() ->
	do_spawn(?LIMIT),
	%%findMax(),
	%%findAvg(),
	%%findMin(),
	%%updateFragment(0 , [5,0]),
	retrieveFragment(trunc(?LIMIT/4)),
	%%register(collector , spawn(?MODULE , collectDeadProcesses , [])),
	io:format("").


%% ---------- Various Operations ----------

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


calculateUpdate(FragmentId , Value) ->
	whereis(p0) ! {update_fragment , FragmentId , Value},
	exit(self() , "end of purpose").

updateFragment(FragmentId , Value) ->
	spawn(?MODULE , calculateUpdate , [FragmentId , Value]).


calculateRetrieve(FragmentId) ->
	whereis(p0) ! {retrieve_fragment , FragmentId},
	exit(self() , "end of purpose").

retrieveFragment(FragmentId) ->
	spawn(?MODULE , calculateRetrieve , [FragmentId]).


%% ---------- Unused Functions ----------


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



%% change according to the secrets
updateMinMaxAvg(NeighborSecret) ->
	MySecret = get('secret'),
	HisSecret = lists:nth(1 , NeighborSecret),
	{_ , HisValue} = HisSecret,
	
	if
		length(MySecret) == (0) ->
			MyNumber = get('number'),
			io:format("~p",[MyNumber]);
			%%Max = findMax(HisValue,MyNumber);
			%%Avg = findAvg2(HisValue,MyNumber);
			%%Min = findMin(HisValue,MyNumber);
		true ->
			MySingleSecret = lists:nth(1 , MySecret),
			{_ , MyValue} = MySingleSecret
			%%Max = findMax(HisValue,MyValue)
			%%Avg = findAvg2(HisValue,MyValue)
			%%Min = findMin(HisValue,MyValue)
	end,

	%%MyNewSecret = [{max , Max}],
	%%MyNewSecret = [{avg , Avg}],
	%%MyNewSecret = [{min , Min}],
	Me = get('number'),
	if
		(Me rem 10) == (0) ->
		     	 %%io:format("Result | ~p | | ~p |~n",[get('name') , Max]);
		     	 %%io:format("Result | ~p | | ~p |~n",[get('name') , Avg]);
		     	 %%io:format("Result | ~p | | ~p |~n",[get('name') , Min]);
			 true;
		true ->
			 io:format("")
	end.
	%%put(secret , (MyNewSecret)).
	

updateFrag(NeighborSecret) ->
	MySecret = get('secret'),
	if
		length(NeighborSecret) /=0 ->
			HisSecret = lists:nth(1 , NeighborSecret),
			{_ , HisId , HisValue} = HisSecret,
			IsSecret = lists:member(HisSecret , MySecret),	
			if
				IsSecret == (false) ->
			 		 MyNewSecret = [HisSecret | MySecret];
				true ->
					MyNewSecret = MySecret
			end,
	 		 MyFragId = get('fragmentId'),
	 		 if
				HisId == (MyFragId) ->
      	 	      		      put(fragmentValue , (HisValue));
				true ->
		      		     io:format("")
	 		end,
			MyNumber = get('number'),
			if
				(MyNumber rem trunc(?LIMIT/4)) == (0) ->
		     			  io:format("Result | ~p | | ~p , ~p | | ~p | | ~p ~p | | ~p |~n",[get('name') , get('fragmentId') , get('fragmentValue') , get('secret') , HisId , HisValue , IsSecret]);
				true ->
				     io:format("")
			end,
			put(secret , (MyNewSecret));
		true ->
		     io:format("")
	end.


buildSecretUpdate(NeighborSecret , 0) ->
	io:format("");
buildSecretUpdate(NeighborSecret , I) ->
	Secret = lists:nth(I , NeighborSecret),
	IsSecret = lists:member(Secret , get('secret')),
	if
		IsSecret == (false) ->
			NewSecret = [{max,get('number')} | get('secret')],
			put(secret , (NewSecret));
		true ->
			io:format("")
	end,
	buildSecretUpdate(NeighborSecret , I-1).
buildSecretMinMaxAvg(NeighborSecret , I) ->
	{Secret , _ , _} = lists:nth(I , NeighborSecret),
	IsSecret = lists:keysearch(Secret , 1 , get('secret')),
	if
		IsSecret == (false) ->
			NewSecret = [{max,get('number')} | get('secret')],
			put(secret , (NewSecret));
		true ->
			io:format("")
	end,
	buildSecretMinMaxAvg(NeighborSecret , I-1).
