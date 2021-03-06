%% ---------- Module Name ----------
-module(spawner).

%% ---------- Compile time arguments to suppress warnings ----------
-compile([nowarn_unused_function , nowarn_unused_vars]).

%% ---------- functions that are exported ----------
-export([start/3 , start/4 , process/3 , findMax/0 , calculateMax/0 , findMin/0 , calculateMin/0 , findSize/0 , calculateSize/0 , findAvg/0 , calculateAvg/0 , findFineAvg/0 , calculateFineAvg/0 , updateFragment/2 , calculateUpdate/2 , retrieveFragment/1 , calculateRetrieve/1 , findMedian/0 , calculateMedian/0 , findFineMedian/0 , calculateFineMedian/0 , collectDeadProcesses/1]).


%% ---------- Definitions ----------

%% Timeout of receive 
-define(TIMEOUT , 1000).

%% ----------


%% ---------------------
%% Update Maximum Secret 
%% ---------------------

%% Find Max of Two
%% ---------------

findMax2(X , Y) ->
	if
		X > Y ->
		  	Max = X;
		true ->
			Max = Y
	end,
	Max.

%% ----------


%% Find Max of Local Fragments
%% ---------------------------

findMyMax(0 , L , Max) ->

	Max;

findMyMax(N , L , Max) ->

	Nth = lists:nth(N , L),
	findMyMax(N-1 , L , findMax2(Max , Nth)).

%% ----------


%% Increaments the Count of Messages
%% ---------------------------------

doMaxUpdate() ->

	MySecret = get('secret'),

	Secret = getOperation(max , MySecret),

	if
		Secret /= (false) ->
			{max , TotalCount , TermCount , Max} = Secret,
			TermLimit = (get('convlimit')),
			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							put(max , (Max)),
							io:format("Max,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Max]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{max , NewTotalCount , NewTermCount , Max} | lists:keydelete(max , 1 , MySecret)]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doMaxUpdate(Secret) ->

	{_ , _ , _ , HisMax} = Secret,

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->
			MyFragValueList = getFragValueList(get('fragment')),
			MyMax = findMyMax(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList)),
 			Max = findMax2(HisMax , MyMax),

			put(secret , ([{max , 0 , 0 , Max} | MySecret]));

		true ->
			{_ , {_ , _ , _ , MyMax}} = lists:keysearch(max , 1 , MySecret),
			Max = findMax2(HisMax , MyMax),
			{_ , TotalCount , TermCount , _} = getOperation(Operation , MySecret),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						Max == (MyMax) ->
				       		        NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									put(max , (Max)),
									io:format("Max,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Max]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{max , NewTotalCount , NewTermCount , Max} | lists:keydelete(max , 1 , MySecret)])

	end.

%% ----------


%% ---------------------
%% Update Minimum Secret
%% ---------------------

%% Find Min of Two
%% ---------------

findMin2(X , Y) ->

	if
		X > Y ->
		        Min = Y;
		true ->
			Min = X
	end,
	Min.

%% ----------


%% Find Local Minimum
%% ------------------

findMyMin(0 , L , Min) ->

	Min;

findMyMin(N , L , Min) ->

	Nth = lists:nth(N , L),
        findMyMin(N-1 , L , findMin2(Min , Nth)).

%% ----------


%% Update the Count of Messages
%% ----------------------------

doMinUpdate() ->

	MySecret = get('secret'),

	Secret = getOperation(min , MySecret),

	if
		Secret /= (false) ->

			{min , TotalCount , TermCount , Min} = Secret,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							put(min , (Min)),
							io:format("Min,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Min]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{min , NewTotalCount , NewTermCount , Min} | lists:keydelete(min , 1 , MySecret)]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doMinUpdate(Secret) ->

	{_ , _ , _ , HisMin} = Secret,

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->

			MyFragValueList = getFragValueList(get('fragment')),
			MyMin = findMyMin(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList)),
			Min = findMin2(HisMin , MyMin),
			put(secret , ([{min , 0 , 0 , Min} | MySecret]));

		true ->

			{_ , {_ , _ , _ , MyMin}} = lists:keysearch(min , 1 , MySecret),
			Min = findMin2(HisMin , MyMin),
			{_ , TotalCount , TermCount , _} = getOperation(Operation , MySecret),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						Min == (MyMin) ->
							NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									put(min , (Min)),
									io:format("Min,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Min]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{min , NewTotalCount , NewTermCount , Min} | lists:keydelete(min , 1 , MySecret)])
	end.

%% ----------


%% ------------------
%% Update Size Secret
%% ------------------

%% List [{FragId , Its Length},{},...]
%% -----------------------------------

getFragSizeList([]) ->

	L = [],
	L;

getFragSizeList([Frag | FragList]) ->

	{Id , Value} = Frag,
	[{Id , length(Value)} | getFragSizeList(FragList)].

%% ----------


%% Total Size of The Seen Frags
%% ----------------------------

getSeenFragSize([]) ->

	0;

getSeenFragSize([SeenFrag | SeenFragList]) ->

	{_ , Len} = SeenFrag,
	Len + getSeenFragSize(SeenFragList).

%% ----------


%% Update the Count of Messages
%% ----------------------------

doSizeUpdate() ->

	Secret = getOperation(size , get('secret')),

	if
		Secret /= (false) ->
			{size , TotalCount , TermCount , Size , SeenFrags} = Secret,
		    TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("Size,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Size]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{size , NewTotalCount , NewTermCount , Size , SeenFrags} | lists:keydelete(size , 1 , get('secret'))]);

		true ->
			true
	end.

%% ----------


%% Update the Values and the Count of Messages
%% -------------------------------------------

doSizeUpdate(Secret) ->

	{_ , _ , _ , HisSize , HisSeenFrags} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->

			MySeenFrags = getFragSizeList(get('fragment')),
			SeenFrags = mergeSeenFrags(HisSeenFrags , MySeenFrags),
			Size = getSeenFragSize(SeenFrags),
			put(secret , ([{size , 0 , 0 , Size , SeenFrags} | get('secret')]));

		true ->

			{_ , {_ , _ , _ , MySize , MySeenFrags}} = lists:keysearch(size , 1 , get('secret')),
			SeenFrags = mergeSeenFrags(HisSeenFrags , MySeenFrags),
			Size = getSeenFragSize(SeenFrags),
			{_ , TotalCount , TermCount , _ , _} = getOperation(Operation , get('secret')),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						Size == (MySize) ->
							NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									io:format("Size,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Size]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , ([{size , NewTotalCount , NewTermCount , Size , SeenFrags} | lists:keydelete(size , 1 , get('secret'))]))
	end.

%% ----------


%% ---------------------
%% Update Average Secret
%% ---------------------

%% Find Avg of Two
%% ---------------

findAvg2(X , Xlen , Y , Ylen) ->

	((X * Xlen) + (Y * Ylen)) / (Xlen + Ylen).

%% ----------


%% Check if Change in Average is Acceptable
%% ----------------------------------------

isNegligibleChange(Avg , MyAvg) ->
	Diff = abs(Avg - MyAvg),
	AvgAccuracy = (get('avgaccuracy')),
	if
		Diff < (AvgAccuracy) ->
		       	true;
		true ->
			false
	end.

%% ----------


%% Update the Count of Messages
%% ----------------------------

doAvgUpdate() ->

	MySecret = get('secret'),

	Secret = getOperation(avg , MySecret),

	if
		Secret /= (false) ->
			{avg , TotalCount , TermCount , Avg , Len} = Secret,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("Avg,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Avg]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{avg , NewTotalCount , NewTermCount , Avg , Len} | lists:keydelete(avg , 1 , MySecret)]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doAvgUpdate(Secret) ->

	{_ , _ , _ , HisAvg , HisLen} = Secret,

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->

			MyFragValueList = getFragValueList(get('fragment')),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, MyFragValueList)) / length(MyFragValueList),
			MyLen = length(MyFragValueList),
			Avg = findAvg2(HisAvg , HisLen , MyAvg , MyLen),
			put(secret , ([{avg , 0 , 0 , Avg , MyLen} | MySecret]));

		true ->

			{_ , {_ , _ , _ , MyAvg , MyLen}} = lists:keysearch(avg , 1 , MySecret),
			Avg = findAvg2(HisAvg , HisLen , MyAvg , MyLen),
			{_ , TotalCount , TermCount , _ , _} = getOperation(Operation , MySecret),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					IsNegligibleChange = isNegligibleChange(Avg , MyAvg),

					if
						IsNegligibleChange == (true) ->
							NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									io:format("Avg,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Avg]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{avg , NewTotalCount , NewTermCount , Avg , MyLen} | lists:keydelete(avg , 1 , MySecret)])
	end.

%% ----------


%% --------------------------
%% Update Fine Average Secret
%% --------------------------

%% Returns List [{FragId , Sum of its numbers , Length of Frag} , {} , ...]
%% ------------------------------------------------------------------------

getFragSumLenList([]) ->

	L = [],
	L;

getFragSumLenList([Frag | FragList]) ->

	{Id , Value} = Frag,
	[{Id , lists:sum(Value) , length(Value)} | getFragSumLenList(FragList)].

%% ----------


%% Gives Total Sum of The Seen Frags
%% ---------------------------------

getSeenFragSum([]) ->

	0;

getSeenFragSum([SeenFrag | SeenFragList]) ->

	{_ , Sum , _} = SeenFrag,
	Sum + getSeenFragSum(SeenFragList).

%% ----------


%% Gives the Total Length of Seen Frags
%% ------------------------------------

getSeenFragLen([]) ->

	0;

getSeenFragLen([SeenFrag | SeenFragList]) ->

	{_ , _ , Len} = SeenFrag,
	Len + getSeenFragLen(SeenFragList).

%% ----------


%% Merge Two Lists of Seen Frags
%% -----------------------------

mergeSeenFrags(HisSeenFrags , []) ->

	HisSeenFrags;

mergeSeenFrags(HisSeenFrags , [Frag | MySeenFrags]) ->

	case lists:member(Frag , HisSeenFrags) of
		false ->
			[Frag | mergeSeenFrags(HisSeenFrags , MySeenFrags)];
		true ->
			mergeSeenFrags(HisSeenFrags , MySeenFrags)
	end.

%% ----------


%% Updates the Count of Messages
%% -----------------------------

doFineAvgUpdate() ->

	Secret = getOperation(fine_avg , get('secret')),

	if
		Secret /= (false) ->
			{fine_avg , TotalCount , TermCount , Avg , SeenFrags} = Secret,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("FineAvg,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Avg]);
						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{fine_avg , NewTotalCount , NewTermCount , Avg , SeenFrags} | lists:keydelete(fine_avg , 1 , get('secret'))]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doFineAvgUpdate(Secret) ->

	{_ , _ , _ , HisAvg , HisSeenFrags} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->

			MySeenFrags = getFragSumLenList(get('fragment')),
			SeenFrags = mergeSeenFrags(HisSeenFrags , MySeenFrags),
			Len = getSeenFragLen(SeenFrags),
			Avg = getSeenFragSum(SeenFrags) / Len,
			put(secret , ([{fine_avg , 0 , 0 , Avg , SeenFrags} | get('secret')]));

		true ->

			{_ , {_ , _ , _ , MyAvg , MySeenFrags}} = lists:keysearch(fine_avg , 1 , get('secret')),
			SeenFrags = mergeSeenFrags(HisSeenFrags , MySeenFrags),
			Len = getSeenFragLen(SeenFrags),
			Avg = getSeenFragSum(SeenFrags) / Len,
			{_ , TotalCount , TermCount , _ , _} = getOperation(Operation , get('secret')),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					IsNegligibleChange = isNegligibleChange(Avg , MyAvg),

					if
						IsNegligibleChange == (true) ->
						NewTermCount = TermCount + 1,

						if
							NewTermCount == (TermLimit) ->
							    io:format("FineAvg,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Avg]);

							true ->
								true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , ([{fine_avg , NewTotalCount , NewTermCount , Avg , SeenFrags} | lists:keydelete(fine_avg , 1 , get('secret'))]))
	end.

%% ----------


%% -----------------------------
%% Update Fragment Update Secret
%% -----------------------------

%% Updates the Count of Messages
%% -----------------------------

doUpdateFragUpdate() ->
	MySecret = get('secret'),
	Secret = getOperation(update_frag , MySecret),

	if
		Secret /= (false) ->
			{_ , TotalCount , TermCount , Id , Value} = Secret,
			TermLimit = (get('convlimit')),
			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("Update,~p,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Id , Value]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{update_frag , NewTotalCount , NewTermCount , Id , Value} | lists:keydelete(update_frag , 1 , MySecret)]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doUpdateFragUpdate(Secret) ->

	Operation = element(1, Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->

			{_ , _ , _ , HisId , HisValue} = Secret,
			MyFragIdList = getFragIdList(get('fragment')),
			IsPresent = lists:member(HisId , MyFragIdList),			

	 		if
				IsPresent == (true) ->
					NewFragment = [{HisId , HisValue} | lists:keydelete(HisId , 1 , get('fragment'))],
					put(fragment , (NewFragment));

				true ->
		      		     true
	 		end,

			NewSecret = {update_frag , 0 , 0 , HisId , HisValue},
			put(secret , ([Secret | MySecret]));

		true ->

			{_ , {_ , TotalCount , TermCount , Id , Value}} = lists:keysearch(update_frag , 1 , MySecret),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						length(Value) /= 0 ->
				       		        NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									    io:format("Update,~p,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Id , Value]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{update_frag , NewTotalCount , NewTermCount , Id , Value} | lists:keydelete(update_frag , 1 , MySecret)]),
			true
	end.

%% ----------


%% --------------------------------
%% Update Fragment Retrieval Secret 
%% --------------------------------

%% Updates the Count of Messages
%% -----------------------------

doRetrieveFragUpdate() ->

	MySecret = get('secret'),

	Secret = getOperation(retrieve_frag , MySecret),

	if
		Secret /= (false) ->
			{retrieve_frag , TotalCount , TermCount , FragId , FragValue} = Secret,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("Retrieve,~p,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , FragId , FragValue]);
						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{retrieve_frag , NewTotalCount , NewTermCount , FragId , FragValue} | lists:keydelete(retrieve_frag , 1 , MySecret)]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doRetrieveFragUpdate(Secret) ->

	{_ , _ , _ , HisFragId , HisFragValue} = Secret,

	MyFragIdList = getFragIdList(get('fragment')),

	IdPresent = lists:member(HisFragId , MyFragIdList),

	IsPresent = getMatch(Secret , get('secret')),

	case  IsPresent of

		false ->

			if
				IdPresent == (true) ->
					{_ , {_ , MyFragValue}} = lists:keysearch(HisFragId , 1 , get('fragment')),
					put(secret , ([{retrieve_frag , 0 , 0 , HisFragId , MyFragValue} | get('secret')]));

				true ->
					put(secret , ([{retrieve_frag , 0 , 0 , HisFragId , HisFragValue} | get('secret')]))
			end;

		_ ->

			if
				IdPresent == (true) ->
					{_ , {_ , MyFragValue}} = lists:keysearch(HisFragId , 1 , get('fragment')),
					FragValue = MyFragValue;

				true ->

					if
						length(HisFragValue) /=0 ->
							FragValue = HisFragValue;

						true ->
							{_ , _ , _ , _ , MyFragValue} = IsPresent,
							FragValue = MyFragValue
					end
			end,

			{_ , TotalCount , TermCount , _ , PrevFragValue} = IsPresent,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						FragValue == (PrevFragValue) ->
				       		        NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									io:format("Retrieve,~p,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , HisFragId , FragValue]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , ([{retrieve_frag , NewTotalCount , NewTermCount , HisFragId , FragValue} | lists:filter(fun(X) -> X /= (IsPresent) end, get('secret'))]))
	end.

%% ----------


%% --------------------
%% Update Median Secret
%% --------------------

%% Find Local Median
%% -----------------

findMedian(List) ->

	SortedList = lists:sort(List),

	Length = length(SortedList),

	case (Length rem 2) of

		 0 ->

			Med1 = trunc(Length / 2),
			Med2 = Med1 + 1,
			Median = (lists:nth(Med1 , SortedList) + lists:nth(Med2 , SortedList)) / 2;

		1 ->

			Med = trunc((Length + 1) / 2),
			Median = lists:nth(Med , SortedList)
	end,

	Median.

%% ----------


%% Merge the New and Prev Frags
%% ----------------------------

mergeFragLists(L , []) ->

	L;

mergeFragLists(L1 , [E | L2]) ->

	case lists:member(E , L1) of
		false ->
			L3 = [E | L1];
		true ->
			L3 = L1
	end,

	mergeFragLists(L3 , L2).

%% ----------


%% Gives List of Numbers in Frag
%% -----------------------------

getValueList([] , ValueList) ->

	ValueList;

getValueList([E | L] , ValueList) ->

	{_ , Value} = E,
	NewValueList = lists:append(Value , ValueList),
	getValueList(L , NewValueList).

%% ----------


%% Updates the Count of Messages
%% -----------------------------

doMedianUpdate() ->

	Secret = getOperation(median , get('secret')),

	if

		Secret /= (false) ->
			{median , TotalCount , TermCount , Median , MyFragList} = Secret,
		    TermLimit = (get('convlimit')),

		    if
		       	TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							io:format("Median,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Median]);

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{median , NewTotalCount , NewTermCount , Median , MyFragList} | lists:keydelete(median , 1 , get('secret'))]);

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doMedianUpdate(Secret) ->

	{_ , _ , _ , _ , HisFragList} = Secret,

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->

			MyFragments = get('fragment'),
			MergedList = mergeFragLists(MyFragments , HisFragList),
			MergedValueList = getValueList(MergedList , []),
			MyMedian = findMedian(MergedValueList),
			put(secret , ([{median , 0 , 0 , MyMedian , MergedList} | get('secret')]));

		true ->

			{_ , {_ , _ , _ , MyMedian , MyFragList}} = lists:keysearch(median , 1 , get('secret')),
			MergedList = mergeFragLists(MyFragList , HisFragList),
			MergedValueList = getValueList(MergedList , []),
			Median = findMedian(MergedValueList),

			{_ , TotalCount , TermCount , _ , _} = getOperation(Operation , get('secret')),
			TermLimit = (get('convlimit')),
			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,

					if
						Median == (MyMedian) ->
							NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									io:format("Median,~p,~p,~p,~p~n" , [get('name') , NewTotalCount , NewTermCount , Median]);

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , [{median , NewTotalCount , NewTermCount , Median , MergedList} | lists:keydelete(median , 1 , get('secret'))])
	end.

%% ----------


%% -------------------------
%% Update Fine Median Secret
%% -------------------------

%% Get Secret That Matches FneMedian and the Round
%% -----------------------------------------------

getFineMedianSecretMatch(Secret , []) ->

	false;

getFineMedianSecretMatch(Secret , [MySecret | MySecrets]) ->

	HisRound = element(4 , Secret),
	MyRound = element(4 , MySecret),
	MyOperation = element(1 , MySecret),

	case ((MyOperation == (fine_median)) and (MyRound == (HisRound))) of
		true ->
			MySecret;
		false ->
			getFineMedianSecretMatch(Secret , MySecrets)
	end.

%% ----------


%% Gives The Count From the Counts for Each Frag
%% ---------------------------------------------

getLessThanCount([]) ->
	0;

getLessThanCount([Pair | Pairs]) ->
	{_ , Count} = Pair,
	Count + getLessThanCount(Pairs).

%% ----------


%% Number of Values Less than Mid in a Frag
%% ----------------------------------------

getLessThan(Mid , []) ->
	0;

getLessThan(Mid , [Value | Values]) ->
	case (Mid < Value) of
	     	false ->
			1 + getLessThan(Mid , Values);
		true ->
			getLessThan(Mid , Values)
	end.

%% ----------


%% List of Frag and Number of Values less than Mid
%% -----------------------------------------------

getLessThanList(Mid , []) ->

	[];

getLessThanList(Mid , [Frag | Frags]) ->
	{Id , Value} = Frag,
	[{Id,getLessThan(Mid , Value)} | getLessThanList(Mid , Frags)].

%% ----------


%% Updates the Count of Messages
%% -----------------------------

doFineMedianUpdate() ->

	Secret = getOperation(fine_median , get('secret')),

	if
		Secret /= (false) ->
			{_ , TotalCount , TermCount , MyRound , Min , Max , MyN , LessThan , LessThanList , BestCombi} = Secret,
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					NewTermCount = TermCount + 1,

					if
						NewTermCount == (TermLimit) ->
							{BestMin , BestMax , BestCount} = BestCombi,

							if
								((BestCount =< (MyN/2)) and (BestCount >= LessThan)) ->
									self() ! {trigger_finemedian , MyRound+1 , BestMin , BestMin , MyN , BestCombi};

								true ->

									if
										LessThan < ((MyN/2)) ->
											self() ! {trigger_finemedian , MyRound+1 , (Max+Min)/2 , Max , MyN , {Min , Max , LessThan}};
										LessThan > ((MyN/2)) ->
											self() ! {trigger_finemedian , MyRound+1 , Min , (Max+Min)/2 , MyN , {Min , Max , LessThan}};
										true ->
											self() ! {got_median , {Min , Max , LessThan}}
									end
							end;

						true ->
							true
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , ([{fine_median , NewTotalCount , NewTermCount , MyRound , Min , Max , MyN , LessThan , LessThanList , BestCombi} | lists:keydelete(fine_median , 1 , get('secret'))]));

		true ->
			true
	end.

%% ----------


%% Updates the Values and the Count of Messages
%% --------------------------------------------

doFineMedianUpdate(Secret) ->

	{_ , _ , _ , Round , HisMin , HisMax , N , HisLessThan , HisLessThanList , _} = Secret,

	IsMatch = getFineMedianSecretMatch(Secret , get('secret')),

	case IsMatch of

		false ->
			true;

		_ ->

			{_ , TotalCount , TermCount , MyRound , MyMin , MyMax , MyN , MyLessThan , MyLessThanList , BestCombi} = IsMatch,
			LessThanList = mergeSeenFrags(HisLessThanList , MyLessThanList),
			LessThan = getLessThanCount(LessThanList),
			Min = findMin2(HisMin , MyMin),
			Max = findMax2(HisMax , MyMax),
			TermLimit = (get('convlimit')),

			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					IsNegligibleChange = isNegligibleChange(LessThan , MyLessThan),

					if
						IsNegligibleChange == (true) ->
							NewTermCount = TermCount + 1,

							if
								NewTermCount == (TermLimit) ->
									{BestMin , BestMax , BestCount} = BestCombi,
									if
										((BestCount =< (MyN/2)) and (BestCount >= LessThan)) ->
											self() ! {trigger_finemedian , MyRound+1 , BestMin , BestMin , MyN , BestCombi};

										true ->

											if
												LessThan < ((MyN/2)) ->
													self() ! {trigger_finemedian , MyRound+1 , (Max+Min)/2 , Max , MyN , {Min , Max , LessThan}};
												LessThan > ((MyN/2)) ->
													self() ! {trigger_finemedian , MyRound+1 , Min , (Max+Min)/2 , MyN , {Min , Max , LessThan}};
												true ->
													self() ! {got_median , {Min , Max , LessThan}}
											end
									end;

								true ->
									true
							end;

						true ->
							NewTermCount = 0
					end;

				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,

			put(secret , ([{fine_median , NewTotalCount , NewTermCount , MyRound , Min , Max , MyN , LessThan , LessThanList , BestCombi} | lists:keydelete(fine_median
 , 1 , get('secret'))]))

	end.

%% ----------


%% -------------------------------------
%% Update Secrets Depending on Responses
%% -------------------------------------

update() ->
	doMaxUpdate(),
	doMinUpdate(),
	doSizeUpdate(),
	doAvgUpdate(),
	doFineAvgUpdate(),
	doUpdateFragUpdate(),
	doRetrieveFragUpdate(),
	doMedianUpdate(),
	doFineMedianUpdate().


update([]) ->
	true;

update([Secret | RemainingSecret]) ->
	
	Operation = element(1,Secret),

	case Operation of
		max ->
		        doMaxUpdate(Secret);
		min ->
		        doMinUpdate(Secret);
		size ->
			doSizeUpdate(Secret);
		avg ->
			doAvgUpdate(Secret);
		fine_avg ->
		        doFineAvgUpdate(Secret);
		update_frag ->
		        doUpdateFragUpdate(Secret);
		retrieve_frag ->
		        doRetrieveFragUpdate(Secret);
		median ->
		        doMedianUpdate(Secret);
		fine_median ->
		        doFineMedianUpdate(Secret);
		_ ->
		        true
	end,
		
	update(RemainingSecret).

%% ----------


%% --------------------
%% Build Maximum Secret
%% --------------------

doMaxBuild(Secret) ->

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->
			MyFragValueList = getFragValueList(get('fragment')),
			MyMax = findMyMax(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList)),
			put(secret , ([{max , 0 , 0 , MyMax} | MySecret]));
		true ->
			true
	end.

%% ----------


%% --------------------
%% Build Minimum Secret
%% --------------------

doMinBuild(Secret) ->

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

	        false ->
			MyFragValueList = getFragValueList(get('fragment')),
			MyMin = findMyMin(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList)),
			put(secret , ([{min , 0 , 0 , MyMin} | MySecret]));
		true ->
			true
	end.

%% ----------


%% -----------------
%% Build Size Secret
%% -----------------

doSizeBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->
			MyFragValueList = getFragValueList(get('fragment')),
			SeenFrags = getFragSizeList(get('fragment')),
			MySize = getSeenFragSize(SeenFrags),
			put(secret , ([{size , 0 , 0 , MySize , SeenFrags} | get('secret')]));
		true ->
			true
	end.

%% ----------


%% --------------------
%% Build Average Secret
%% --------------------

doAvgBuild(Secret) ->

	Operation = element(1 , Secret),

	MySecret = get('secret'),

	case lists:member(Operation , getOperationList(MySecret)) of

		false ->
			MyFragValueList = getFragValueList(get('fragment')),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, MyFragValueList)) / length(MyFragValueList),
			MyLen = length(MyFragValueList),
			put(secret , ([{avg , 0 , 0 , MyAvg , MyLen} | MySecret]));
		true ->
			true
	end.

%% ----------


%% -------------------------
%% Build Fine Average Secret
%% -------------------------

doFineAvgBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->
			MyFragValueList = getFragValueList(get('fragment')),
			SeenFrags = getFragSumLenList(get('fragment')),
			MyLen = getSeenFragLen(SeenFrags),
			MyAvg = getSeenFragSum(SeenFrags) / MyLen,
			put(secret , ([{fine_avg , 0 , 0 , MyAvg , SeenFrags} | get('secret')]));
		true ->
			true
	end.

%% ----------


%% ------------------------------
%% Build Fragment Retrieve Secret
%% ------------------------------

%% Find Secret with Same Frag Id
%% -----------------------------
getMatch(_, []) ->

        false;

getMatch(Secret , [MySecret | MyRemainingSecret]) ->

	Operation = element(1 , MySecret),
	if
		Operation == (retrieve_frag) ->
			{HisSec , _ , _ , HisFragId , _} = Secret,
			{MySec , _ , _ , MyFragId , _} = MySecret,

        	case  ((HisSec == (MySec)) and (HisFragId == (MyFragId)))of
	        	true ->
		    	    MySecret;
				false ->
		        	getMatch(Secret , MyRemainingSecret)
			end;
		true ->
			getMatch(Secret , MyRemainingSecret)
	end.

%% ----------


%% ----------
doRetrieveFragBuild(Secret) ->

	{_ , _ , _ , HisFragId , HisFragValue} = Secret,

	MyFragIdList = getFragIdList(get('fragment')),

	IdPresent = lists:member(HisFragId , MyFragIdList),

	IsPresent = getMatch(Secret , get('secret')),

	case  IsPresent of

	        false ->
			if
				IdPresent == (true) ->
					{_ , {_ , MyFragValue}} = lists:keysearch(HisFragId , 1 , get('fragment')),
					put(secret , ([{retrieve_frag , 0 , 0 , HisFragId , MyFragValue} | get('secret')]));
				true ->
					put(secret , ([{retrieve_frag , 0 , 0 , HisFragId , HisFragValue} | get('secret')]))
			end;
		_ ->
			if
				IdPresent == (true) ->
					{_ , {_ , MyFragValue}} = lists:keysearch(HisFragId , 1 , get('fragment')),
					FragValue = MyFragValue;
				true ->
					if
						length(HisFragValue) /=0 ->
							FragValue = HisFragValue;
					true ->
						{_ , _ , _ , _ , MyFragValue} = IsPresent,
						FragValue = MyFragValue
					end
			end,
			{_ , TotalCount , TermCount , _ , PrevFragValue} = IsPresent,
			TermLimit = (get('convlimit')),
			if
				TermCount < (TermLimit) ->
					NewTotalCount = TotalCount + 1,
					if
						FragValue == (PrevFragValue) ->
				       		        NewTermCount = TermCount + 1,
							if
								NewTermCount == (TermLimit) ->
									io:format("ReF | ~p | | ~p |~n" , [get('name') , get('secret')]);
								true ->
									true
							end;
						true ->
							NewTermCount = 0
					end;
				true ->
					NewTotalCount = TotalCount,
					NewTermCount = TermCount
			end,
			put(secret , ([{retrieve_frag , NewTotalCount , NewTermCount , HisFragId , FragValue} | lists:filter(fun(X) -> X /= (IsPresent) end, get('secret'))]))
	end.

%% ----------


%% -------------------
%% Build Median Secret
%% -------------------

doMedianBuild(Secret) ->

	Operation = element(1 , Secret),

	case lists:member(Operation , getOperationList(get('secret'))) of

		false ->
			MyMedian = findMedian(getFragValueList(get('fragment'))),
			put(secret , ( [ {median , 0 , 0 , MyMedian , get('fragment')} | get('secret') ] ) );
		true ->
			true;
		_ ->
			true
	end.

%% ----------


%% ------------------------
%% Build Fine Median Secret
%% ------------------------

doFineMedianBuild(NeighborSecret) ->

	IsPresent = lists:keysearch(fine_median , 1 , NeighborSecret),
	case IsPresent of
		false ->
			L = [];
		_ ->
			{_ , {_ , _ , _ , Round , Min , Max , N , LessThan , LessThanList , _}} = IsPresent,
			MyLessThanList = getLessThanList(((Min+Max)/2) , get('fragment')),
			MyLessThanCount = getLessThanCount(MyLessThanList),
			L = [{fine_median , 0 , 0 , Round , Min , Max , N , MyLessThanCount , MyLessThanList , {Min , Max , MyLessThanCount}}]
	end,
	L.

%% ----------


%% -------------------------------
%% Build The Secret To Be Returned
%% -------------------------------

build([]) ->
	true;

build([Secret | RemainingSecret]) ->

        Operation = element(1,Secret),

	case Operation of
	        max ->
		        doMaxBuild(Secret);
		min ->
		        doMinBuild(Secret);
		size ->
			doSizeBuild(Secret);
		avg ->
			doAvgBuild(Secret);
		fine_avg ->
		        doFineAvgBuild(Secret);
		retrieve_frag ->
			doRetrieveFragBuild(Secret);
		median ->
			doMedianBuild(Secret);
		_ ->
		        true
	end,
		
	build(RemainingSecret).

%% -----------


%% ----
%% Push 
%% ----

%% Wait For Response To Push
%% -------------------------

waitPushResponse() ->

	receive

		{push_response , NeighborSecret} ->
			if
				length(NeighborSecret) /=0 ->
					update(NeighborSecret);
				true ->
					update(),
					true
			end

	after ?TIMEOUT ->
	      	true
	end.

%% ----------


%% Send Push Request
%% -----------------

push() ->
	Neighbor = selectNeighbor(),
	Name = get('name'),
	if
		Name /= Neighbor ->
			Secret = getLiveSecrets(get('secret')),
			if
				length(Secret) /= 0 ->
					Neighbor ! {push_request , Secret , Name},
					waitPushResponse();
				true ->
					true
			end;
		true ->
			true
	end,
	true.

%% ----------


%% ----
%% Pull
%% ----

%% Wait For Pull Completion
%% ------------------------

waitPullComplete() ->
	receive

		{pull_complete , NeighborSecret} ->
			%%io:format("| ~p | going to update", [get('name')]),
			if
				length(NeighborSecret) /=0 ->
		        		update(NeighborSecret);
				true ->
					update(),
					true
			end

	after ?TIMEOUT ->
	      	true

	end.

%% ----------


%% Wait for Pull Response
%% ----------------------

waitPullResponse() ->
	receive

		{pull_response , NeighborSecret , Neighbor} ->
			build(NeighborSecret),
			Secret = lists:merge(getLiveSecrets(get('secret')) , doFineMedianBuild(NeighborSecret)),
			%%io:format("| ~p | sending pull_complete~n| ~p |~n| ~p |~n~n~n",[get('name'),get('secret') , Secret]),
			whereis(Neighbor) ! {pull_complete , Secret},
			if
				length(NeighborSecret) /=0 ->
		        		update(NeighborSecret);
				true ->
					update(),
					true
			end;

		 no_secret ->
			true

	after ?TIMEOUT ->
		true

	end.

%% ----------


%% Send Pull Request
%% -----------------

pull() ->
	Neighbor = selectNeighbor(),
	Name = get('name'),
	if
		Neighbor /= Name ->
			 %%io:format("| ~p |'s Pull~n",[get('name')]),
			 whereis(Neighbor) ! {pull_request , Name},
			 waitPullResponse();
		true ->
			 true
	end,
	true.

%% ----------


%% -------------------------
%% Push And Pull Alternation
%% -------------------------

pushpull() ->

	Phase = get('phase'),

	if
		Phase == (push) ->
			Secret = getLiveSecrets(get('secret')),
			if
				length(Secret) /= (0) ->
					case (get('steps')) of
						0 ->
							push();
						_ ->
							true
					end,
					put(steps , (get('steps') rem get('steplimit'))),
					true;

				true ->
					true
			end,
			put(phase , (pull));

		true ->
			%%timer:sleep(2000),
			pull(),
			put(phase , (push))
	end,
	true.

%% ----------


%% ------------------------------------------
%% Listen And PushPull Alternation of Process
%% ------------------------------------------

listen() ->

	pushpull(),

	receive

		%% Message - Pull Request

		{pull_request , Neighbor} ->
			%%io:format("| ~p | : pull request from | ~p | | ~p | ~p |~n",[get('name') , Neighbor , whereis(Neighbor) , registered()]),
			Secret = getLiveSecrets(get('secret')),
			if
				length(Secret) == (0) ->
					whereis(Neighbor) ! no_secret,
					true;
				true ->
					%%io:format("| ~p | doing pull_response~n| ~p |~n| ~p |~n~n~n",[get('name') , get('secret') , Secret]),
					whereis(Neighbor) ! {pull_response , Secret , get('name')},
					waitPullComplete()
			end,
			listen();

		%% Message - Push Request

		{push_request , NeighborSecret , Neighbor} ->
			build(NeighborSecret),
			Secret = lists:merge(getLiveSecrets(get('secret')) , doFineMedianBuild(NeighborSecret)),
			%%io:format("| ~p | doing pull_response~n| ~p |~n| ~p |~n~n~n",[get('name') , get('secret') , Secret]),
			Neighbor ! {push_response ,Secret},
			%%io:format("| ~p | going to update", [get('name')]),
			if
				length(NeighborSecret) /= 0 ->
					update(NeighborSecret);
				true ->
					update(),
					true
			end,
			listen();

		%% Message - Max

		find_max ->
			MyFragValueList = getFragValueList(get('fragment')),
			NewSecret = [{max , 0 , 0 , findMyMax(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList))} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		%% Message - Min

		find_min ->
			MyFragValueList = getFragValueList(get('fragment')),
			NewSecret = [{min , 0 , 0 , findMyMin(length(MyFragValueList) , MyFragValueList , lists:nth(1 , MyFragValueList))} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		%% Message - Size
		
		find_size ->
			MyFragValueList = getFragValueList(get('fragment')),
			SeenFrags = getFragSizeList(get('fragment')),
			MySize = getSeenFragSize(SeenFrags),
			put(secret , ([{size , 0 , 0 , MySize , SeenFrags} | get('secret')])),
			listen();

		%% Message - Avg

		find_avg ->
			MyFragValueList = getFragValueList(get('fragment')),
			MyAvg = (lists:foldl(fun(X, Sum) -> X + Sum end, 0, MyFragValueList)) / length(MyFragValueList),
			MyLen = length(MyFragValueList),
			put(secret , ([{avg , 0 , 0 , MyAvg , MyLen} | get('secret')])),
			listen();

		%% Message - Fine Avg

		find_fineavg ->
			MyFragValueList = getFragValueList(get('fragment')),
			SeenFrags = getFragSumLenList(get('fragment')),
			MyLen = getSeenFragLen(SeenFrags),
			MyAvg = getSeenFragSum(SeenFrags) / MyLen,
			put(secret , ([{fine_avg , 0 , 0 , MyAvg , SeenFrags} | get('secret')])),
			listen();

		%% Message - Update Fragment

		{update_fragment , FragmentId , Value} ->
			MyFragIdList = getFragIdList(get('fragment')),
			IsPresent = lists:member(FragmentId , MyFragIdList),
			if
				IsPresent == (true) ->
					NewFragment = [{FragmentId , Value} | lists:keydelete(FragmentId , 1 , get('fragment'))],
					put(fragment , (NewFragment));
				true ->
					 true
			end,
			put(secret , ([{update_frag , 0 , 0 , FragmentId , Value} | get('secret')])),
			listen();

		%% Message - Retrieve Fragment

		{retrieve_fragment , FragmentId} ->
			MyFragIdList = getFragIdList(get('fragment')),
			IsPresent = lists:member(FragmentId , MyFragIdList),
			if
				IsPresent == (true) ->
					MyFragments = get('fragment'),
				        {_ , {MyFragId , MyFragValue}} = lists:keysearch(FragmentId , 1 , MyFragments),
					put(secret , ([{retrieve_frag , 0 , 0 , MyFragId , MyFragValue} | get('secret')]));
				true ->
					put(secret , ([{retrieve_frag , 0 , 0 , FragmentId , []} | get('secret')]))					
			end,
			listen();

		%% Message - Median

		find_median ->
			NewSecret = [ {median , 0 , 0 , findMedian(getFragValueList(get('fragment'))) , get('fragment')} | get('secret')],
			put(secret , (NewSecret)),
			listen();

		%% Message - Fine Median

		find_finemedian ->
			IsMax = get('max'),
			IsMin = get('min'),
			IsSize = get('size'),
			case ((IsMax /= (undefined)) and (IsMin /= (undefined))) of
			     	true ->
					self() ! {trigger_finemedian , (1) , get('min') , get('max') , get('size') , {get('min') , get('max') , 0}};
				false ->
					self() ! find_finemedian
			end,
			selectNeighbor() ! find_finemedian,
			listen();

		{trigger_finemedian , Round , Min , Max , N , BestCombi} ->
			case (get('medianRound') == (Round - 1)) of
			     	true ->
					MedianRoundLimit = get('medianroundlimit'),
					case Round < (MedianRoundLimit) of
					    true ->
							%%io:format("| ~p | karto re baba #~p | ~p |~n",[get('name') , Round , get('secret')]),
							put(medianRound , (Round)),
							Mid = (Min + Max) / 2,
							LessThanList = getLessThanList(Mid , get('fragment')),
							LessThanCount = getLessThanCount(LessThanList),
							MedianLessSecret = getMedianLessSecret(get('secret')),
							put(secret , [{fine_median , 0 , 0 , get('medianRound') , Min , Max , N , LessThanCount , LessThanList , BestCombi} | MedianLessSecret]);
						false ->
							self() ! {got_median , BestCombi}
					end;
				false ->
					true
			end,
			listen();

		{got_median , BestCombi} ->
			{Min , Max , _} = BestCombi,
			io:format("| ~p | Hurray!!! It's | ~p |~n",[get('name') , (Min+Max)/2]),
			listen()

	after ?TIMEOUT ->
	      listen()

	end.

%% ----------


%% ---------------------------------
%% Initialize the Process Dictionary
%% ---------------------------------

init_dict(MyNumber, Limit , MyFrags) ->

	put(number , (MyNumber)),
	put(name , (list_to_atom( string:concat("p" , integer_to_list(MyNumber))))),
	put('limit' , (Limit)),
	put('fraglimit' , (trunc(Limit/2))),
	put('convlimit' , (trunc(3*(math:log(Limit) / math:log(2))))),
	put('avgaccuracy' , ((Limit / 10))),
	put('steplimit' , (trunc((math:log(get('limit'))/math:log(2))))),
	put('medianroundlimit' , (trunc(2*(math:log(get('limit'))/math:log(2))))),
	put(secret , ([])),
	put(steps , 0),
	put(medianRound , 0),
	put(fragment , MyFrags),
	%%put(fragment , [{(MyNumber rem get('fraglimit')) , [(MyNumber rem get('fraglimit')) , ((MyNumber rem get('fraglimit')) + get('fraglimit'))]} , {((MyNumber rem get('fraglimit')) + get('fraglimit')) , [(MyNumber rem get('fraglimit')) + get('limit') , ((MyNumber rem get('fraglimit')) + get('fraglimit')) + get('limit')]}]),
	put(phase , (push)),

	%%io:format("| ~p | | ~p | | ~p | | ~p | | ~p |~n",[get('number') , get('name') , get('neighborList') , get('secret') , get('fragment')]),
	true.

%% ----------


%% ----------------------
%% Entry Point of Process
%% ----------------------

process(MyNumber , Limit , MyFrags) ->

	init_dict(MyNumber , Limit , MyFrags),
	listen(),
	io:format("| ~p | I am exiting",[get('name')]).

%% ----------


%% ---------------------
%% Creation of Processes
%% ---------------------

do_spawn(0 , Limit , _) ->
	true;

do_spawn(N , Limit , Frags) ->
    ProcessName = list_to_atom( string:concat( "p" , integer_to_list( Limit - N ) ) ),
	process_flag(trap_exit, true),
	register(ProcessName , spawn_link(?MODULE , process , [(Limit - N) , Limit , getFragments((Limit - N) , Limit , Frags)])),
	do_spawn(N-1 , Limit , Frags).

%% ----------


%% ------------------
%% Various Operations
%% ------------------

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


calculateSize() ->
	whereis(p0) ! find_size,
	exit(self() , "end of purpose").

findSize() ->
	spawn(?MODULE , calculateSize , []).


calculateAvg() ->
	whereis(p0) ! find_avg,
	exit(self() , "end of purpose").

findAvg() ->
	spawn(?MODULE , calculateAvg , []).


calculateFineAvg() ->
	whereis(p0) ! find_fineavg,
	exit(self() , "end of purpose").

findFineAvg() ->
	spawn(?MODULE , calculateFineAvg , []).


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


calculateMedian() ->
	whereis(p0) ! find_median,
	exit(self() , "end of purpose").

findMedian() ->
	spawn(?MODULE , calculateMedian , []).


calculateFineMedian() ->
	whereis(p0) ! find_max,
	whereis(p0) ! find_min,
	whereis(p0) ! find_size,
	whereis(p0) ! find_finemedian,
	exit(self() , "end of purpose").

findFineMedian() ->
	spawn(?MODULE , calculateFineMedian , []).

%% ----------


%% ----------
%% File Input
%% ----------

getFragList(NumLines , NumTuples , InputFile) ->

     {ok, FileDev} = file:open(InputFile, [raw, read, read_ahead]),
	 T = getFrags([],FileDev,NumLines,NumTuples),
	 file:close(FileDev),
	 T.


getFrags(Frags, _, _,0) ->

	Frags;

getFrags(Frags, FileDev, NumLines, N) ->

	List = do_read([],FileDev, NumLines),
	X = erlang:append_element({N}, List),
	getFrags([X|Frags], FileDev, NumLines,N - 1).


do_read(Lines, _, 0) ->

	 Lines;

do_read(Lines, FileDev, L) ->

     case file:read_line(FileDev) of
          {ok, Line} ->
			   Line1 = re:replace(Line, "(\n)*", "", [global,{return,list}]),
			   case length(Line1) of
			   	0 ->
					do_read(Lines, FileDev, 0);
				_ ->
			   Line2 = list_to_integer(Line1),
			   do_read([Line2|Lines], FileDev, L - 1)
			   end;
          eof ->
               do_read(Lines, FileDev, 0)
     end.


countLines(eof,N) ->

     N;

countLines(FileDev, N) ->

     case file:read_line(FileDev) of
          {ok, Line} ->
			   countLines(FileDev, N + 1);
          eof ->
               countLines(eof, N)
     end.

%% ----------


%% -------------
%% Entry Point 1
%% -------------

%% Arguments : the number of nodes, number of fragments and input file

start(Limit , M , InputFile) ->

	%% Parse the Input File to get fragments

	io:format("~n~nYou can do following computation~n"),
	io:format("~p:findMax()~n",[?MODULE]),
	io:format("~p:findMin()~n",[?MODULE]),
	io:format("~p:findSize()~n",[?MODULE]),
	io:format("~p:findAvg()~n",[?MODULE]),
	io:format("~p:findFineAvg()~n",[?MODULE]),
	io:format("~p:updateFragment(FragId, [Values1,Value2,...])~n",[?MODULE]),
	io:format("~p:retrieveFragment(FragId)~n",[?MODULE]),
	io:format("~p:findMedian()~n",[?MODULE]),
	io:format("~p:findFineMedian()~n~n",[?MODULE]),

	Status = file:open(InputFile, [raw, read, read_ahead]),

	case Status of

		{error , _} ->
			io:format("File not present, please check");

		_ ->
			{_ , FileDev} = Status,
			NumLines = countLines(FileDev, 0),
			file:close(FileDev),
			Frags = getFragList(trunc(NumLines/M) , M , InputFile),

			%% Write the output to a file
			{ok, F} = file:open("out.csv", [write]),
			group_leader(F, self()),

			%% Function to spawn 'Limit' number of processes
			do_spawn(Limit , Limit , Frags)

			%%register(collector , spawn(?MODULE , collectDeadProcesses , [])),
	end,

	true.

%% ----------


%% ---------- Entry Point 2 ----------

%% Arguments : the operation to be performed, the number of nodes, number of fragments and the input file

start(Operation , Limit , M , InputFile) ->

	%% Parse the Input File to get fragments

	Status = file:open(InputFile, [raw, read, read_ahead]),

	case Status of

		{error , _} ->
			io:format("File not present, please check");

		_ ->
			{_ , FileDev} = Status,
			NumLines = countLines(FileDev, 0),
			file:close(FileDev),
			Frags = getFragList(trunc(NumLines/M),M , InputFile),

			%% Write the output to a file
			S = string:concat(atom_to_list(Operation), integer_to_list(Limit)),
			OutputFile = string:concat(S , ".csv"),
			{ok, F} = file:open(OutputFile, [write]),
			group_leader(F, self()),

			%% Function to spawn 'Limit' number of processes
			do_spawn(Limit , Limit , Frags),

			%% Do Operation
			case Operation of
			max ->
				findMax();
			min ->
				findMin();
			size ->
				findSize();
			avg ->
				findAvg();
			fineavg ->
				findFineAvg();
			up ->
				updateFragment(0 , [10 , 20]);
			re ->
				retrieveFragment(0);
			median ->
				findMedian();
			finemedian ->
				findFineMedian();
			_ ->
				io:format("Wrong Operation, Use one of (min , max , avg , fineavg , median , finemedian)~n")
		end

			%%register(collector , spawn(?MODULE , collectDeadProcesses , [Limit]))
	end,
	true.

%% ----------


%% --------------------
%% List of Live Secrets
%% --------------------

getLiveSecrets([]) ->
	T = [],
	T;

getLiveSecrets([Secret | RemainingSecretList]) ->
	TermCount = element(3 , Secret),
	TermLimit = get('convlimit'),
	if
		TermCount < (TermLimit) ->
			L = [Secret | getLiveSecrets(RemainingSecretList)];
		true ->
			L = getLiveSecrets(RemainingSecretList)
	end,
	L.

%% ----------


%% ---------------------
%% Selection of Neighbor
%% ---------------------

selectNeighbor() ->
	random:seed(now()),
	Neighbor = list_to_atom( string:concat( "p" , integer_to_list((random:uniform(get('limit'))-1)))),
	IsAlive = is_process_alive(whereis(Neighbor)),
	if
		IsAlive == (false) ->
			  SelectedNeighbor = selectNeighbor();
		true ->
			  SelectedNeighbor = Neighbor
	end,
	SelectedNeighbor.


selectNeighborOld() ->
	MyNeighbors = getMirrorChordNeighborList(get('number')), %%get('neighborList'),
	random:seed(now()),
	Neighbor = lists:nth(random:uniform(length(MyNeighbors)) , MyNeighbors),
	IsAlive = is_process_alive(whereis(Neighbor)),
	if
		IsAlive == (false) ->
			  SelectedNeighbor = selectNeighbor();
		true ->
			  SelectedNeighbor = Neighbor
	end,
	SelectedNeighbor.


selectNeighborNew() ->
	MyNeighbors = getMirrorChordNeighborList(get('number')) , %%get('neighborList'),
	random:seed(now()),
	Index = random:uniform(2*(length(MyNeighbors))),
	case (Index rem 2) of
		0 ->
			Neighbor = lists:nth(trunc(Index/2) , MyNeighbors),
			IsPresent = lists:member(Neighbor , registered()),
			if
				IsPresent == (false) ->
			  		selectNeighbor();
				true ->
					Neighbor
			end;
	     	1 ->
			get('name')
	end.

%% ----------


%% ----------
%% Topologies
%% ----------

%% -------------
%% Ring Topology
%% -------------

getRingNeighborList(MyNumber) ->

	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((get('limit') + MyNumber - 1) rem get('limit') )))),
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem get('limit') )))),

	NeighborList = [Me , Predecessor , Successor],
	NeighborList.


%% --------------------
%% Mirror Ring Topology
%% --------------------

getMirrorRingNeighborList(MyNumber) ->

	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((get('limit') + MyNumber - 1) rem get('limit') )))),
	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem get('limit') )))),
	T1 = [Me , Predecessor , Successor],

	MirrorMyNumber = (get('limit') - MyNumber - 1),
	MirrorMe = list_to_atom( string:concat( "p" , integer_to_list( MirrorMyNumber ))),
	IsMirrorMe = lists:member(MirrorMe , T1),
	if
		IsMirrorMe == (false) ->
			T2 = [MirrorMe | T1];
		true ->
			T2 = T1
	end,

	MirrorSuccessor = list_to_atom( string:concat( "p" , integer_to_list( ((MirrorMyNumber + 1) rem get('limit') )))),
	IsMirrorSuccessor = lists:member(MirrorSuccessor , T2),
	if
		IsMirrorSuccessor == (false) ->
			T3 = [MirrorSuccessor | T2];
		true ->
			T3 = T2
	end,

	MirrorPredecessor = list_to_atom( string:concat( "p" , integer_to_list( ((get('limit') + MirrorMyNumber - 1) rem get('limit') )))),
	IsMirrorPredecessor = lists:member(MirrorPredecessor , T3),
	if
		IsMirrorPredecessor == (false) ->
			NeighborList = [MirrorPredecessor | T3];
		true ->
			NeighborList = T3
	end,

	NeighborList.

%% --------------


%% --------------
%% Chord Topology
%% --------------

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
	NewList = [list_to_atom( string:concat( "p" , integer_to_list( trunc((MyNumber + math:pow(2,I-1))) rem get('limit') ))) | List],
	getList(MyNumber , NewList , I-1).


getReverseList(MyNumber , List , 0) ->
	List;

getReverseList(MyNumber , List , I) ->
	NewList = [list_to_atom(string:concat("p" , integer_to_list(trunc((MyNumber - math:pow(2,I-1) + get('limit'))) rem get('limit')))) | List],
	getReverseList(MyNumber , NewList , I-1).


%% ------------
%% Mirror Chord
%% ------------

getMirrorChordNeighborList(MyNumber) ->

	Length = ceiling(log2(get('limit'))/2),

	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),

	ChordList = getList(MyNumber , [] , Length),
	ReverseChordList = getReverseList(MyNumber , [] , Length),

	lists:umerge(lists:sort(ChordList) , lists:sort(ReverseChordList)).


%% -----
%% Chord
%% -----

getChordNeighborList(MyNumber) ->
	Length = ceiling(log2(get('limit'))),

	Me = list_to_atom( string:concat( "p" , integer_to_list( MyNumber ))),
	T1 = [Me | getList(MyNumber , [] , Length)],

	Successor = list_to_atom( string:concat( "p" , integer_to_list( ((MyNumber + 1) rem get('limit') )))),
	IsSuccessor = lists:member(Successor , T1),
	if
		IsSuccessor == (false) ->
			T2 = [Successor | T1];
		true ->
			T2 = T1
	end,

	Predecessor = list_to_atom( string:concat( "p" , integer_to_list( ((get('limit') + MyNumber - 1) rem get('limit') )))),
	IsPredecessor = lists:member(Predecessor , T2),
	if
		IsPredecessor == (false) ->
			T3 = [Predecessor | T2];
		true ->
			T3 = T2
	end,

	MirrorMyNumber = (get('limit') - MyNumber - 1),
	MirrorMe = list_to_atom( string:concat( "p" , integer_to_list( MirrorMyNumber ))),
	IsMirrorMe = lists:member(MirrorMe , T3),
	if
		IsMirrorMe == (false) ->
			T4 = [MirrorMe | T3];
		true ->
			T4 = T3
	end,

	MirrorSuccessor = list_to_atom( string:concat( "p" , integer_to_list( ((MirrorMyNumber + 1) rem get('limit') )))),
	IsMirrorSuccessor = lists:member(MirrorSuccessor , T4),
	if
		IsMirrorSuccessor == (false) ->
			T5 = [MirrorSuccessor | T4];
		true ->
			T5 = T4
	end,

	MirrorPredecessor = list_to_atom( string:concat( "p" , integer_to_list( ((get('limit') + MirrorMyNumber - 1) rem get('limit') )))),
	IsMirrorPredecessor = lists:member(MirrorPredecessor , T5),
	if
		IsMirrorPredecessor == (false) ->
			NeighborList = [MirrorPredecessor | T5];
		true ->
			NeighborList = T5
	end,

	NeighborList.

%% ----------


%% ----------
generateFragList(Index , Limit , Frags) ->
	case Index < length(Frags) of
	     	true ->
			[lists:nth(Index+1 , Frags) | generateFragList(Index + Limit , Limit , Frags)];
		false ->
			L = [],
			L
	end.


getFragments(MyNumber , Limit , Frags) ->
	M = length(Frags),
	case M < Limit of
	       true ->
			[lists:nth(((MyNumber rem M)+1) , Frags)];
	       false ->
			generateFragList(MyNumber , Limit , Frags)
	end.

%% ----------


%% -----------------------
%% Listsing Dead Processes
%% -----------------------

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


collectDeadProcesses(Limit) ->

	io:format("Dead | ~p |~n", [checkIfDead(Limit-1 , [])]),
	timer:sleep(3000),
	collectDeadProcesses(Limit).

%% ----------


%% ----------

getMedianLessSecret([]) ->

	L = [],
	L;

getMedianLessSecret([Secret | Secrets]) ->

	Operation = element(1 , Secret),

	case Operation of
	     	fine_median ->
			getMedianLessSecret(Secrets);
		_ ->
			[Secret | getMedianLessSecret(Secrets)]
	end.

%% ----------


%% ----------------------------
%% List of Operations in Secret
%% ----------------------------

getOperationList([]) ->
	L = [],
	L;

getOperationList([Secret | RemainingSecretList]) ->
	Operation = element(1 , Secret),
	[Operation | getOperationList(RemainingSecretList)].


getOperation(Operation , []) ->
	false;

getOperation(Operation , [Secret | RemainingSecretList]) ->
	SecretOperation = element(1, Secret),
	if
		SecretOperation  == (Operation) ->
			Secret;
		true ->
			getOperation(Operation , RemainingSecretList)
	end.

%% ----------


%% --------------------------------------
%% Derive Fragment Value List and Id List
%% --------------------------------------

getFragValueList([]) ->
	N=[],
	N;

getFragValueList([E | L]) ->
	{_ , Value} = E,
	lists:append(Value , getFragValueList(L)).

getFragIdList([]) ->
	N=[],
	N;

getFragIdList([E | L]) ->
	{Id , _} = E,
	[Id | getFragIdList(L)].

%% -----------
