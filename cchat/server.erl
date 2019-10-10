-module(server).
-export([start/1,stop/1]).

-record(state, {
    nicks,   % list of all nicks registered so far
    channels % list of all channels created so far
}).

initial_state() ->
    #state{
        nicks = [],
        channels = []
    }.

%
% handle/2 handles each kind of request from client process.
%
% Parameters:
% (1) St: the current state of the server.
% (2) Request: request data from the client.
%
% Must return a tuple {reply, Data, NewState}, where:
% (1) Data is what that is to be sent back to the client, which must be one of the following:
%       - The atom `ok`
%       - The tuple {error, ErrorAtom, ErrorMessage}
% (2) NewState is the updated state of the server.
%

% Client request to join a channel
handle(St, {join, ClientPid, ClientNick, Channel}) ->
    NewNicksList =
        case lists:member(ClientNick, St#state.nicks) of
            true  -> St#state.nicks;
            false -> [ClientNick | St#state.nicks]
        end,
    NewChannelsList =
        case lists:member(Channel, St#state.channels) of
            true  -> St#state.channels;
            false -> channel:start(Channel), [Channel | St#state.channels]
        end,
    ChannelResponse = genserver:request(list_to_atom(Channel), {join, ClientPid}),
    {reply, ChannelResponse, St#state{nicks=NewNicksList, channels=NewChannelsList}};

% Client request to change nick
handle(St, {nick, OldNick, NewNick}) ->
    case lists:member(NewNick, St#state.nicks) of
        true when OldNick =:= NewNick ->
            {reply, ok, St};
        true ->
            {reply, {error, nick_taken, "Nick "++NewNick++" has been taken!"}, St};
        false ->
            NewNicksList = [NewNick | lists:delete(OldNick, St#state.nicks)],
            {reply, ok, St#state{nicks=NewNicksList}}
        end;

% Client request to quit
handle(St, {quit, ClientNick}) ->
    NewNicksList = lists:delete(ClientNick, St#state.nicks),
    {reply, ok, St#state{nicks=NewNicksList}};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Server cannot handle this request!"}, St} .

% Start a new server process with the given name.
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).

% Stop the server process registered to the given name.
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
