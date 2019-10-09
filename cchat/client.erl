-module(client).
-export([handle/2, initial_state/3]).

% TODO: Think about how to reuse the same thing for the 3 functions
% TODO: Where to insert server_not_reached?

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% Send a request to the server, and wait to return a response.
send(St, Request) ->
    try genserver:request(St#client_st.server, Request) of
        Response -> Response
    catch
        error:_ -> {error, server_not_reached, "Server not reached!"}
        % TODO: What about other types (exit, throw)?
    end.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case send(St, {join, self(), St#client_st.nick, Channel}) of
        {error, ErrorAtom, ErrorMsg} ->
            {reply, {error, ErrorAtom, ErrorMsg}, St};
        ok ->
            {reply, ok, St}
        end;

% Leave channel
handle(St, {leave, Channel}) ->
    case send(St, {leave, self(), Channel}) of
        {error, ErrorAtom, ErrorMsg} ->
            {reply, {error, ErrorAtom, ErrorMsg}, St};
        ok ->
            {reply, ok, St}
        end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case send(St, {message_send, self(), Channel, Msg}) of
        {error, ErrorAtom, ErrorMsg} ->
            {reply, {error, ErrorAtom, ErrorMsg}, St};
        ok ->
            {reply, ok, St}
        end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
% TODO
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
