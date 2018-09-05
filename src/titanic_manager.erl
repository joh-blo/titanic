%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(titanic_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 reset/0,
	 check_project/1,check_file/2,
	 status/0,status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("titanic_internal.hrl").

-define(SERVER, ?MODULE).

-record(state,{
	  projs::list(),  % All projects we depend on
	  files::list(),  % All files part of the main project
	  configs::list(),% All configs
	  status::list()  % Key/Value list with current status
	 }).

-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


reset() ->
    gen_server:call(?MODULE,{reset,all}).

status() ->
    gen_server:call(?MODULE,{status,all}).

status(Key) ->
    gen_server:call(?MODULE,{status,Key}).

check_project(ProjPath) ->
    gen_server:cast(?MODULE,{check_proj,ProjPath}).

check_file(FilePath,ProjName) ->
    gen_server:cast(?MODULE,{check_file,FilePath,ProjName}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{projs=[],
		files=[],
		configs=[],
		status=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({reset,all}, _From, State) ->
    %% Read the current status    
    Reply=ok,
    {reply, Reply, State#state{files=[],projs=[],configs=[],status=[]}};
handle_call({status,Key}, _From, State=#state{status=Status}) ->
    %% Read the current status    
    Reply=if
	      Key==validate ->
		  proplists:get_value(validate,Status,true)
	  end,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({check_file,FilePath,ProjName}, State=#state{files=Files,
							 status=Status}) ->
    %% Checks if:
    %% - The referd file exists.
    %% - We refer to the same file in differnt locations.
    FileName=filename:basename(FilePath),
    {NewFiles,NewStatus}=
	case lists:keysearch(FileName,1,Files) of
	    false ->
		%% A new file, check if it exists!
		NS=case file:read_file_info(FilePath) of
		       {ok,_} ->
			   true;
		       _ ->
			   
			   io:format("WARNING: ~p is missing!~n",[FilePath]),
			   false
		   end,
		{[{FileName,ProjName}|Files],update_status(NS,validate,Status)};
	    {value,{_,ProjName}} ->
		{Files,Status};
	    {value,{_,OldProjName}} ->
		io:format("WARNING:"
			  " Found ~p in ~p, but previously also in ~p~n",
			  [FileName,ProjName,OldProjName]),
		{Files,update_status(false,validate,Status)}
	end,
    {noreply, State#state{files=NewFiles,status=NewStatus}};
handle_cast({check_proj,ProjPath},State=#state{projs=Projs,status=Status}) ->
    ProjName=filename:basename(ProjPath),
    {NewProjs,NewStatus}=
	case lists:keysearch(ProjName,1,Projs) of
	    false ->
		%% A new file, check if it exists!
		NS=case file:read_file_info(ProjPath) of
		       {ok,_} ->
			   true;
		       _ ->
			   io:format("WARNING: ~p is missing!~n",
				     [ProjPath]),
			   false
		   end,
		{[{ProjName,ProjPath}|Projs],update_status(NS,validate,Status)};
	    {value,{_,ProjPath}} ->
		{Projs,Status};
	    {value,{_,OldProjPath}} ->
		io:format("WARNING: Found ~p project referenced both at~n"
			  "  ~p~n"
			  "AND~n"
			  "  ~p~n",
			  [ProjName,ProjPath,OldProjPath]),
		{Projs,Status}
	end,
    {noreply, State#state{projs=NewProjs,status=NewStatus}}.


update_status(true,_Key,Status) ->
    Status;
update_status(NS,Key,Status) ->
    case lists:keysearch(Key,1,Status) of
	false ->
	    [{Key,NS}|Status];
	{value,{_,NS}} ->
	    Status;
	_ ->
	    lists:keyreplace(Key,1,Status,{Key,NS})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Increase the value of the "Key" counter with one.
%% increase_cntr([],Key,undefined,Out) ->
%%     {1,lists:reverse([{Key,1}|Out])};
%% increase_cntr([],_Key,CntVal,Out) ->
%%     {CntVal,lists:reverse(Out)};
%% increase_cntr([{Key,Val}|Rest],Key,_CntVal,Out) ->
%%     increase_cntr(Rest,Key,Val+1,[{Key,Val+1}|Out]);
%% increase_cntr([H|Rest],Key,CntVal,Out) ->
%%     increase_cntr(Rest,Key,CntVal,[H|Out]).


    
