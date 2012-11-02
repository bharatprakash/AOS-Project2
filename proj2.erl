-module(proj2).
-export([start_server/0, server/1, start_process/1, stop_process/0, message/2, process/2]).

server_node() ->
    proj2@manishpc.

server(User_List) ->
    receive
        {From, start, Name} ->
            New_User_List = server_start_process(From, Name, User_List), server(New_User_List);
        {From, stop} ->
            New_User_List = server_stop_process(From, User_List), server(New_User_List)
    end.

%%% Start the server
start_server() ->
    register(server, spawn(proj2, server, [[]])).


%%% Server adds a new user to the user list
server_start_process(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},  %reject logon
            User_List;
        false ->
            From ! {messenger, process_started},
            [{From, Name} | User_List]        %add user to the list
    end.

%%% Server deletes a user from the user list
server_stop_process(From, User_List) ->
    lists:keydelete(From, 1, User_List).


%%% User Commands
start_process(Name) ->
    case whereis(process) of 
        undefined -> 
            register(process, spawn(proj2, process, [server_node(), Name]));
        _ -> already_logged_on
    end.

stop_process() ->
    process ! stop.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
end.


%%% The client process which runs on each server node
process(Server_Node, Name) ->
    {server, Server_Node} ! {self(), start, Name},
    await_result(),
    process(Server_Node).

process(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    process(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        {messenger, stop, Why} -> % Stop the client 
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->  % Normal response
            io:format("~p~n", [What])
    end.
