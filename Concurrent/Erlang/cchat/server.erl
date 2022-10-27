-module(server).
-export([start/1, stop/1]).

% -- Start function --

% Start a new server process with the given name
% Do not change the signature of this function.

start(ServerAtom) ->
  Users = genserver:start(users, [], fun userHandler/2),
  genserver:start(ServerAtom, {[],Users}, fun handle/2).

% -- Stop function --

% Stop the server process registered to the given name,
% together with any other associated processes

stop(ServerAtom) ->
  genserver:request(ServerAtom, {kill_channels}),
  genserver:stop(users),
  genserver:stop(ServerAtom).
% -- user handler


userHandler(Users,{newNick, Client, Nick})->

    case lists:keymember(Client, 1, Users) of
      false ->case lists:keymember(Nick, 2, Users) of
                  false ->  
                   {reply, newNick, [{Client,Nick}|Users]};
                  true -> {reply, failed, [{Client,"Nickless" }|Users]}
              end;

      true ->  case lists:keymember(Nick, 2, Users) of
                  false ->  
                   {reply, newNick, [{Client,Nick}|lists:keydelete(Client, 1, Users)]};
                  true -> {reply, failed, Users}
              end
  end.


% -- Handle function --

handle (St, {kill_channels})->
  {Channels, _} = St,
  lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, Channels),
  {reply, ok, []};


handle (St, {nick, Client, Nick})->
  {_, Users} = St,
  case genserver:request(Users, {newNick, Client, Nick}) of
    newNick->{reply, newNick, St};
    failed-> {reply , nick_taken , St}
    end
  ;
% Case for command join -
% Checks first to see if channel exists in list 
% of channels and assign client to said channel

handle(St, {join, Channel, Client}) ->
     {Channellist, Users} = St,

  % Channel exists? Else start new channel.
  case lists:member(Channel, Channellist) of
    true ->
      case genserver:request(list_to_atom(Channel), {join, Client}) of
        joined -> {reply, joined, St};
        failed -> {reply, failed, St}
      end;
    false ->
      genserver:start(list_to_atom(Channel), [Client], fun channel/2),
      {reply, joined, {[Channel | Channellist],Users}}
  end.



% -- Channel function --

% Case for command join -
% Checks first to see if client already in channel. 
% Else add client to lists of clients.

channel(Clients, {join, Client}) ->
  % Already in channel?
  case lists:member(Client, Clients) of
    true -> {reply, failed, Clients};
    false -> {reply, joined, [Client | Clients]}
  end;

% Case for command leave -
% Checks first to see if client is in the channel.
% Then removes from lists of clients.

channel(Clients, {leave, Client}) ->
  % Connected to channel?
  case lists:member(Client, Clients) of
    true -> {reply, ok, lists:delete(Client, Clients)};
    false -> {reply, never_joined, [Clients]}
  end;

% Case for message -
% Checks first to see if sending client is in the channel. 

channel(Clients, {message, Channel, Nick, Msg, Sender}) ->
  % Connected to channel?
  case lists:member(Sender, Clients) of
    true ->
      % Removing sender from list of receiving clients
      Pids = lists:delete(Sender, Clients),
      % Spawn process for message
      [spawn(fun() -> genserver:request(Pid,
        {message_receive, Channel, Nick, Msg}) end) || Pid <- Pids],
      {reply, sent, Clients};
    false -> {reply, failed, Clients}
  end.