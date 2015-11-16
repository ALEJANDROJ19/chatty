-module(client).
%% Exported Functions
-export([start/2]).

%% API Functions
start(ServerPid, MyName) ->
    ClientPid = spawn(fun() -> init_client(ServerPid, MyName) end),
    process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
    %% Connecta el client al servidor. Envia el seu Name i From.
    ServerPid ! {client_join_req, MyName, self()},
    process_requests().

%% Local Functions
%% Funcions amb les que treballa el protocol del chat.
process_requests() ->
    receive
        {join, Name} ->
            %% Un client amb nom Name s'ha conectat al chat.
            io:format("[JOIN] ~s joined the chat~n", [Name]),
            process_requests();
        {leave, Name} ->
            %% Un client amb nom Name s'ha desconnectat del chat
            io:format("[LEAVE] ~s leaved the chat~n", [Name]),
            process_requests();
        {message, Name, Text} ->
            %% El client Name ha enviat el missatge Text en el chat.
            io:format("[~s] ~s", [Name, Text]),
            process_requests();
        exit ->
            %% Desconnecta el client.
            ok
    end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
    %% Llegir i interpretar l'entrada estandard
    Text = io:get_line("-> "),
    if 
        Text  == "exit\n" ->
            %% Si l'usuari escriu exit, desconectar el client del servidor.
            ServerPid ! {client_leave_req, MyName, ClientPid},
            ok;
        true ->
            %% Qualsevol altre cossa que escrigui, enviar com a missatge.
            ServerPid ! {send, MyName, Text},
            process_commands(ServerPid, MyName, ClientPid)
    end.