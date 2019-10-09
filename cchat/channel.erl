-module(channel).
-export([start/1,stop/1]).

-record(state, {
    name,   % name of the channel
    members % list of members' pids
}).

initial_state(Name) ->
    #state{
        name = Name,
        members = []
    }.

%
% handle/2 handles each kind of request from another process.
%
% Parameters:
% (1) St: the current state of the channel.
% (2) Request: request data from the requesting process.
%
% Must return a tuple {reply, Data, NewState}, where:
% (1) Data is what that is to be sent back, which must be one of the following:
%       - The atom `ok`
%       - The tuple {error, ErrorAtom, ErrorMessage}
% (2) NewState is the updated state of the channel.
%

% Client request to join
handle(St, {join, ClientPid}) ->
    case lists:member(ClientPid, St#state.members) of
        true ->
            % Client has already joined
            {reply, {error, user_already_joined, "Already joined "++St#state.name}, St};
        false ->
            % Client has not joined
            NewMembersList = [ClientPid | St#state.members],
            {reply, ok, St#state{members=NewMembersList}}
        end;

% Client request to leave
handle(St, {leave, ClientPid}) ->
    % Assume client has already joined
    NewMembersList = lists:delete(ClientPid, St#state.members),
    {reply, ok, St#state{members=NewMembersList}};

% Client request to send message to everyone else
handle(St, {message_send, ClientPid, ClientNick, Msg}) ->
    % Assume client has already joined
    OtherMembers = lists:delete(ClientPid, St#state.members),
    Data = {request, self(), make_ref(), {message_receive, St#state.name, ClientNick, Msg}},
    lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Channel cannot handle this request!"}, St} .

% Start a new channel process with the given name.
start(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

% Stop the channel process registered to the given name.
stop(Name) ->
    genserver:stop(list_to_atom(Name)).
