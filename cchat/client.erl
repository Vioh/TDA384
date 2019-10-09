-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
-record(state, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % list of channels the client is currently part of
}).

% Return an initial state record. This is called from GUI.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #state{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

% Send a request to the destination, and wait to return a response.
send(DestinationAtom, Request) ->
    try genserver:request(DestinationAtom, Request) of
        Response -> Response
    catch
        error:_ -> {error, server_not_reached, "Server not reached!"}
    end.

%
% handle/2 handles each kind of request from GUI.
%
% Parameters:
% (1) St: the current state of the client.
% (2) Request: request data from GUI.
%
% Must return a tuple {reply, Data, NewState}, where:
% (1) Data is what that is to be sent back to GUI, which must be one of the following:
%       - The atom `ok`
%       - The tuple {error, ErrorAtom, ErrorMessage}
% (2) NewState is the updated state of the client.
%

% Join channel
handle(St, {join, Channel}) ->
    case send(St#state.server, {join, self(), St#state.nick, Channel}) of
        ok ->
            NewChannelsList = [Channel | St#state.channels],
            {reply, ok, St#state{channels=NewChannelsList}};
        Error ->
            {reply, Error, St}
        end;

% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#state.channels) of
        true ->
            ChannelReply = send(list_to_atom(Channel), {leave, self()}),
            NewChannelsList = lists:delete(Channel, St#state.channels),
            {reply, ChannelReply, St#state{channels=NewChannelsList}};
        false ->
            {reply, {error, user_not_joined, "Not a member of channel "++Channel}, St}
        end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel, St#state.channels) of
        true ->
            ChannelReply = send(list_to_atom(Channel), {message_send, self(), St#state.nick, Msg}),
            {reply, ChannelReply, St};
        false ->
            {reply, {error, user_not_joined, "Not a member of channel "++Channel}, St}
        end;

% Change to new nick (if not taken)
handle(St, {nick, NewNick}) ->
    case send(St#state.server, {nick, St#state.nick, NewNick}) of
        ok ->
            {reply, ok, St#state{nick=NewNick}};
        Error ->
            {reply, Error, St}
    end;

% Get current nick
handle(St, whoami) ->
    {reply, St#state.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #state{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
    % Leave all channels
    lists:foreach(
        fun(Channel) -> list_to_atom(Channel) ! {request, self(), make_ref(), {leave, self()}} end,
        St#state.channels
    ),
    % Tell server about the quit
    St#state.server ! {request, self(), make_ref(), {quit, St#state.nick}},
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
