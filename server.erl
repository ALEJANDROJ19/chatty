-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    %% Iniciar el servidor
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    %% Funcions amb les que treballa el protocol del chat per al server
    receive
        {client_join_req, Name, From} ->
            %% Un nou client s'ha connectat al servidor. Ho anunciem a tots els clients.
            NewClients = [From|Clients],
                %% Guardem l'identificador, no l'username!
            broadcast(NewClients, {join, Name}),
                %% Guardem la nova llista d'usuaris
            process_requests(NewClients);
        {client_leave_req, Name, From} ->
            %% Un client s'ha desconnectat del servidor. Ho anunciem a tots els clients
            NewClients = lists:delete(From, Clients), %% Eliminar client de la llista de clients del server.
            broadcast(NewClients, {leave,Name}),  %% Difusió del missatge
            From ! exit, %% Desconnexió del client per seguretat.
            process_requests(NewClients);
        {send, Name, Text} ->
            %% Missatge rebut, fem difussió d'aquest a tots els clients.
            broadcast(Clients, {message,Name,Text}),
            process_requests(Clients);
        disconnect ->
            %% Desconnectar el servidor.
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
