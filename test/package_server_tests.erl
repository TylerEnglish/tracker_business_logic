-module(package_server_tests).
-include_lib("eunit/include/eunit.hrl").

-record(state, {packages = #{}, locations = #{}}).

%% Create a default initial state
initial_state() ->
    #state{packages = #{}, locations = #{}}.

%% Setup function to start the GenServer
setup() ->
    package_server:start_link().

%% Teardown function
teardown(_) ->
    ok.

package_transferred_test_() ->
    {setup, fun setup/0, fun teardown/1, fun test_package_transferred/0}.

test_package_transferred() ->
    %% Test valid input
    ?_assertEqual({reply, ok, #state{packages = #{1 => 1}}}, package_server:handle_call({package_transferred, 1, 1}, self(), initial_state())),
    
    %% Test invalid PackID
    ?_assertEqual({reply, {error, invalid_ids}, #state{}}, package_server:handle_call({package_transferred, undefined, 1}, self(), initial_state())),
    
    %% Test invalid LocID
    ?_assertEqual({reply, {error, invalid_ids}, #state{}}, package_server:handle_call({package_transferred, 1, undefined}, self(), initial_state())),
    
    %% Test both PackID and LocID invalid
    ?_assertEqual({reply, {error, invalid_ids}, #state{}}, package_server:handle_call({package_transferred, undefined, undefined}, self(), initial_state())).

delivered_test_() ->
    {setup, fun setup/0, fun teardown/1, fun test_delivered/0}.

test_delivered() ->
    %% Set up initial state with a package
    InitialState = initial_state(),
    {reply, ok, NewState} = package_server:handle_call({package_transferred, 1, 1}, self(), InitialState),
    
    %% Test delivering a valid package
    ?_assertEqual({reply, ok, NewState#state{packages = #{}}}, package_server:handle_call({delivered, 1}, self(), NewState)),
    
    %% Test delivering an invalid package
    ?_assertEqual({reply, {error, invalid_id}, NewState}, package_server:handle_call({delivered, undefined}, self(), NewState)).

location_request_test_() ->
    {setup, fun setup/0, fun teardown/1, fun test_location_request/0}.

test_location_request() ->
    %% Set up initial state with a package and location
    InitialState = initial_state(),
    {reply, ok, State1} = package_server:handle_call({package_transferred, 1, 1}, self(), InitialState),
    {reply, ok, NewState} = package_server:handle_call({location_update, 1, 10.0, 20.0}, self(), State1),
    
    %% Test valid location request
    ?_assertEqual({reply, {ok, 10.0, 20.0}, NewState}, package_server:handle_call({location_request, 1}, self(), NewState)),
    
    %% Test invalid location request
    ?_assertEqual({reply, {error, package_not_found}, NewState}, package_server:handle_call({location_request, undefined}, self(), NewState)).

location_update_test_() ->
    {setup, fun setup/0, fun teardown/1, fun test_location_update/0}.

test_location_update() ->
    %% Test valid input
    ?_assertEqual({reply, ok, #state{locations = #{1 => {10.0, 20.0}}}}, package_server:handle_call({location_update, 1, 10.0, 20.0}, self(), initial_state())),
    
    %% Test invalid LocID
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, undefined, 10.0, 20.0}, self(), initial_state())),
    
    %% Test invalid Longitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, 1, undefined, 20.0}, self(), initial_state())),
    
    %% Test invalid Latitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, 1, 10.0, undefined}, self(), initial_state())),
    
    %% Test invalid LocID and Longitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, undefined, undefined, 20.0}, self(), initial_state())),
    
    %% Test invalid LocID and Latitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, undefined, 10.0, undefined}, self(), initial_state())),
    
    %% Test invalid Longitude and Latitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, 1, undefined, undefined}, self(), initial_state())),
    
    %% Test invalid LocID, Longitude, and Latitude
    ?_assertEqual({reply, {error, invalid_location}, #state{}}, package_server:handle_call({location_update, undefined, undefined, undefined}, self(), initial_state())).
