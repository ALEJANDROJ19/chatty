-module(server2).
%% Exported Functions
-export([start/0, start/1]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> init_server() end),
    register(myserver, ServerPid).

start(BootServer) ->
    ServerPid = spawn(fun() -> init_server(BootServer) end),
    register(myserver, ServerPid).

init_server() ->
    process_requests([], [self()]).

init_server(BootServer) ->
    %% Per unir-se a un servidor que ja està operatiu
    BootServer ! {server_join_req, self()},
    process_requests([], []). %% Cal esperar a que el servidor ens envii la llista de servidors actius.

process_requests(Clients, Servers) ->
    %% Funcions amb les que treballa el protocol del chat per al server
    receive
        %% Missatges entre Client i Servidor
        {client_join_req, Name, From} ->
            %% Un nou client s'ha connectat al servidor. Ho anunciem a tots els clients.
            NewClients = [From|Clients],  %% Afegir client a la llista de Clients
            broadcast(Servers, {join, Name}),  %% Difusió del nou client en tots els Servidors
            process_requests(NewClients, Servers);
        {client_leave_req, Name, From} ->
            %% Un client s'ha desconnectat del servidor. Ho anunciem a tots els clients
            NewClients = lists:delete(From, Clients),  %% Eliminem el client de la llista d'usuaris.
            broadcast(Servers, {leave, Name}),  %% Difusió del client eliminat en tots els Servidors.
            From ! exit, %% Desconnaxió del client per seguretat.
            process_requests(NewClients, Servers);  %% Nova llista de clients
        {send, Name, Text} ->
            %% Missatge rebut, fem difussió d'aquest a tots els clients en tots els Servidors.
            broadcast(Servers, {message, Name, Text}),
            process_requests(Clients, Servers);
        
        %% Messages entre Servidors
        disconnect ->
            %% El servidor es vol desconnectar
            NewServers = lists:delete(self(), Servers),  %% Eliminar el servidor de la llista de Servers
            broadcast(NewServers, {update_servers, NewServers}),  %% Difussió de la nova taula de Servidors a tots els altres
            unregister(myserver); %% Aturar servidor
        {server_join_req, From} ->
            %% Un nou servidor es vol unir al grup de Servidors
            NewServers = [From|Servers],  %% L'afegim a la llista de Servidors
            broadcast(NewServers, {update_servers, NewServers}),  %% Difussió de la nova llista.
            process_requests(Clients, NewServers);
        {update_servers, NewServers} ->
            %% Es rep una nova taula de Servidors actualment connectats.
            io:format("[SERVER UPDATE] ~w~n", [NewServers]),
            process_requests(Clients, NewServers);
            
        RelayMessage -> %% Qualsevol altre missatge és enviat en difussió als clients.
            broadcast(Clients, RelayMessage),
            process_requests(Clients, Servers)
    end.

%% Local Functions
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
