-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [From|Clients],  %% TODO: COMPLETE ## DONE!
            %% Guardem l'identificador, no l'username!
            broadcast(NewClients, {join, Name}),
            %% Guardem la nova llista d'usuaris
            process_requests(NewClients);  %% TODO: COMPLETE ## DONE!
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From, Clients),  %% TODO: COMPLETE ## DONE!
            broadcast(Clients, {leave,Name}),  %% TODO: COMPLETE ## DONE!
            From ! exit,
            process_requests(NewClients);  %% TODO: COMPLETE ## DONE!
        {send, Name, Text} ->
            broadcast(Clients, {message,Name,Text}),  %% TODO: COMPLETE ## DONE!
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
