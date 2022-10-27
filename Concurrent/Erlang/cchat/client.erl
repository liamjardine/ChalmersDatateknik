-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
  gui, % atom of the GUI process
  nick, % nick/username of the client
  server, % atom of the chat server
  channels % List of channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
  #client_st{
    gui = GUIAtom,
    nick = Nick,
    server = ServerAtom,
    channels = []
  }
.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom ok or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client


% -- Handle function --

% Case for join -
% Sending request to join given channel.
% Then adding channel to clients list of channels.

handle(St, {join, Channel}) ->
  % Server active?
  case lists:member(St#client_st.server, registered()) of
    true ->
      try case genserver:request(St#client_st.server, {join, Channel, self()}) of
        joined ->
          {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
        failed ->
          {reply, {error, user_already_joined, "Already in the chatroom."}, St}
        end
        catch throw:timeout_error ->  
          {reply, {error, server_not_reached, "Server unavailible."}, St}
      end;

    false -> {reply, {error, server_not_reached, "Server unavailible."}, St}
  end;

% Case for leave -
% Sending request to the leave channel.
% Then deleting channel from clients list of channels.

handle(St, {leave, Channel}) ->
  % Connected to channel?

  case lists:member(Channel, St#client_st.channels) of
    true -> 
      try case genserver:request(list_to_atom(Channel), {leave, self()}) of
        ok -> {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}};
        never_joined -> {reply, {error, user_not_joined, "Not in channel"}, St}
      end
      catch throw:timeout_error ->  
        {reply, {error, server_not_reached, "Server unavailible."}, St}
      end;
    false -> 
      {reply, {error, user_not_joined, "Not in channel"}, St}
  end;

% Case for message -
% Sending message (from GUI, to channel)

handle(St, {message_send, Channel, Msg}) ->
  % Connected to channel?
  case lists:member(list_to_atom(Channel), registered()) of
    true ->
    try 
        case genserver:request(list_to_atom(Channel), {message, Channel, St#client_st.nick, Msg, self()}) of
          sent -> {reply, ok, St};
          failed -> {reply, {error,user_not_joined, "Not in channel"}, St}
        end
    catch throw:timeout_error ->  
      {reply, {error,server_not_reached, "Server unavailible."}, St}
    end;
    false->{reply, {error,server_not_reached, "Server unavailible."}, St} end;



% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St};

% Change nick 
handle(St, {nick, NewNick}) ->
   try case genserver:request(St#client_st.server, {nick, self(), NewNick}) of
    newNick -> {reply, ok, St#client_st{nick = NewNick}};
    nick_taken -> {reply,{error, nick_taken, "Nickname taken"}, St}
  end
  catch throw:timeout_error ->  {reply, {error,server_not_reached, "Server unavailible."}, St} end;
      

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
  gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
  {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
  % Any cleanup should happen here, but this is optional
  {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _Data) ->
  {reply, {error, not_implemented, "Client does not handle this command"}, St}.