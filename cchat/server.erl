-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    c2m, % dictionary mapping every channel to its members (represented by the PIDs)
    m2n  % dictionary mapping every member (represented by the PID) to its nick name
}).

initial_state() ->
    #server_st{
        c2m = dict:new(),
        m2n = dict:new()
    }.

handle(St, {join, ClientPid, ClientNick, Channel}) ->
    case dict:find(Channel, St#server_st.c2m) of
        {ok, Members} ->
            % Channel has been created
            case lists:member(ClientPid, Members) of
                true ->
                    % Client has already joined
                    {reply, {error, user_already_joined, "User already joined "++Channel}, St};
                false ->
                    % Client has not joined
                    NewC2M = dict:append(Channel, ClientPid, St#server_st.c2m),
                    NewM2N = dict:store(ClientPid, ClientNick, St#server_st.m2n),
                    {reply, ok, St#server_st{c2m=NewC2M, m2n=NewM2N}}
                end;
        error ->
            % Channel has not been created
            NewC2M = dict:store(Channel, [ClientPid], St#server_st.c2m),
            NewM2N = dict:store(ClientPid, ClientNick, St#server_st.m2n),
            {reply, ok, St#server_st{c2m=NewC2M, m2n=NewM2N}}
        end;

handle(St, {leave, ClientPid, Channel}) ->
    case dict:find(Channel, St#server_st.c2m) of
        {ok, Members} ->
            % Channel has been created
            case lists:member(ClientPid, Members) of
                true ->
                    % Client has already joined
                    NewC2M = dict:store(Channel, lists:delete(ClientPid, Members), St#server_st.c2m),
                    {reply, ok, St#server_st{c2m=NewC2M}};
                false ->
                    % Client has not joined
                    {reply, {error, user_not_joined, "User has not joined "++Channel}, St}
                end;
        error ->
            % Channel has not been created
            {reply, {error, user_not_joined, "User has not joined "++Channel}, St}
        end;

handle(St, {message_send, ClientPid, Channel, Msg}) ->
    case dict:find(Channel, St#server_st.c2m) of
        {ok, Members} ->
            % Channel has been created
            case lists:member(ClientPid, Members) of
                true ->
                    % Client has already joined
                    ClientNick = dict:fetch(ClientPid, St#server_st.m2n),
                    OtherMembers = lists:delete(ClientPid, Members),
                    Data = {request, self(), make_ref(), {message_receive, Channel, ClientNick, Msg}},
                    lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
                    {reply, ok, St};
                false ->
                    % Client has not joined
                    {reply, {error, user_not_joined, "User has not joined "++Channel}, St}
                end;
        error ->
            % Channel has not been created
            {reply, {error, user_not_joined, "User has not joined "++Channel}, St}
        end;

% TODO: Think about the atom here
handle(St, _) ->
    {reply, {error, unknown_request, "Unknown request"}, St}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
