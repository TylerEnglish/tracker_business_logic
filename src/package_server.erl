-module(package_server).
-behaviour(gen_server).

%% Include logger for logging
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, package_transferred/2, delivered/1, location_request/1, location_update/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-record(state, {packages = #{}, locations = #{}}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

package_transferred(PackID, LocID) ->
    gen_server:call(?MODULE, {package_transferred, PackID, LocID}).

delivered(PackID) ->
    gen_server:call(?MODULE, {delivered, PackID}).

location_request(PackID) ->
    gen_server:call(?MODULE, {location_request, PackID}).

location_update(LocID, Lon, Lat) ->
    gen_server:call(?MODULE, {location_update, LocID, Lon, Lat}).
         
%% gen_server callbacks
init([]) ->
    logger:info("Initializing package server"),
    {ok, #state{}}.
        
handle_call({package_transferred, PackID, LocID}, _From, State) ->
    logger:info("Transferring package ~p to location ~p", [PackID, LocID]),
    case validate_ids(PackID, LocID) of
        true ->
            NewPackages = maps:put(PackID, LocID, State#state.packages),
            {reply, ok, State#state{packages = NewPackages}};
        false ->
            {reply, {error, invalid_ids}, State}
    end;
           
handle_call({delivered, PackID}, _From, State) ->
    logger:info("Package ~p delivered", [PackID]),
    case maps:is_key(PackID, State#state.packages) of
        true ->
            NewPackages = maps:remove(PackID, State#state.packages),
            {reply, ok, State#state{packages = NewPackages}};
        false ->
            {reply, {error, invalid_id}, State}
    end;
          
handle_call({location_request, PackID}, _From, State) ->
    logger:info("Location request for package ~p", [PackID]),
    case maps:find(PackID, State#state.packages) of
        {ok, LocID} ->
            case maps:find(LocID, State#state.locations) of
                {ok, {Lon, Lat}} ->
                    {reply, {ok, Lon, Lat}, State};
                error ->
                    {reply, {error, location_not_found}, State}
            end;
        error ->
            {reply, {error, package_not_found}, State}
    end;
         
handle_call({location_update, LocID, Lon, Lat}, _From, State) ->
    logger:info("Updating location ~p to (~p, ~p)", [LocID, Lon, Lat]),
    case validate_location(LocID, Lon, Lat) of
        true ->
            NewLocations = maps:put(LocID, {Lon, Lat}, State#state.locations),
            {reply, ok, State#state{locations = NewLocations}};
        false ->
            {reply, {error, invalid_location}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helper functions
validate_ids(PackID, LocID) when is_integer(PackID) andalso is_integer(LocID) ->
    true;
validate_ids(_, _) ->
    false.

validate_location(_LocID, Lon, Lat) when is_number(Lon) andalso is_number(Lat) ->
    true;
validate_location(_, _, _) ->
    false.
