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
	 reset/0,reset/1,
	 init_set/1,
	 insert_project/2,
	 insert_file/3,
	 diff/2,
	 status/0,status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("titanic_internal.hrl").

-define(SERVER, ?MODULE).

-record(state,{
	  projs::list(),  % All projects we depend on
	  sets::list(),   % All sets, where each set consists of [{file,proj}..]
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


init_set(Set) ->
    gen_server:call(?MODULE,{init_set,Set}).

reset() ->
    gen_server:call(?MODULE,{reset,all}).
reset(Set) ->
    gen_server:call(?MODULE,{reset,Set}).

status() ->
    gen_server:call(?MODULE,{status,all}).

status(Key) ->
    gen_server:call(?MODULE,{status,Key}).

insert_project(ProjPath,Set) ->
    gen_server:cast(?MODULE,{insert_proj,ProjPath,Set}).

insert_file(FilePath,ProjName,Set) ->
    gen_server:cast(?MODULE,{insert_file,FilePath,ProjName,Set}).

diff(Set1,Set2) ->
    gen_server:cast(?MODULE,{diff,Set1,Set2}).


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
		sets=[],
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
handle_call({init_set,Set}, _From, State=#state{sets=Sets}) ->
    %% Read the current status
    NewSets=case proplists:get_value(Set,Sets) of
		undefined ->
		    [{Set,[]}|Sets];
		_ ->
		    lists:keyreplace(Set,1,Sets,{Set,[]})
	    end,
    Reply=ok,
%    io:format("INIT SET ~p~n",[Set]),
    {reply, Reply, State#state{sets=NewSets}};
handle_call({reset,all}, _From, State) ->
    Reply=ok,
%    io:format("RESET ALL SETS~n",[]),
    {reply, Reply, State#state{sets=[],projs=[],configs=[],status=[]}};
handle_call({reset,Set}, _From, State=#state{sets=Sets}) ->
    Reply=ok,
    NewSets=case proplists:get_value(Set,Sets) of
		undefined ->
		    Sets;
		_ ->
		    lists:keyreplace(Set,1,Sets,{Set,[]})
	    end,
%    io:format("RESET SET ~p~n",[Set]),
    {reply, Reply, State#state{sets=NewSets,projs=[],configs=[],status=[]}};
handle_call({status,Key}, _From, State=#state{status=Status,
					      sets=Sets,
					      projs=Projs,
					      configs=Cfgs}) ->
    %% Read the current status    
    Reply=case Key of
	      sets ->
		  Sets;
	      projs ->
		  Projs;
	      configs ->
		  Cfgs;
	      validate ->
		  proplists:get_value(validate,Status,true);
	      all ->
		  Status
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
handle_cast({insert_file,FilePath,ProjName,Set},
	    State=#state{sets=Sets,status=Status}) ->
    %% Checks if:
    %% - The file exists.
    %% - We refer to the same file in differnt locations.
    Files=proplists:get_value(Set,Sets),
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
%    io:format("FilePath=~p~n ProjName=~p~n",[FilePath,ProjName]),
		{[{FileName,FilePath,ProjName}|Files],
		 update_status(NS,validate,Status)};
	    {value,{_,_,ProjName}} ->
		{Files,Status};
	    {value,{_,_,OldProjName}} ->
		io:format("WARNING:"
			  " Found ~p in ~p, but previously also in ~p~n",
			  [FileName,ProjName,OldProjName]),
		{Files,update_status(false,validate,Status)}
	end,
    NewSets=lists:keyreplace(Set,1,Sets,{Set,NewFiles}),
%    io:format("NewSets ~p~n",[NewSets]),

    {noreply, State#state{sets=NewSets,status=NewStatus}};
handle_cast({diff,Set1,Set2}, State=#state{sets=Sets,status=Status}) ->
    NewStatus=case equal_sets(Set1,Set2,Sets) of
		  false ->
		      update_status(false,validate,Status);
		  true ->
		      Status
	      end,
    {noreply, State#state{status=NewStatus}};
handle_cast({insert_proj,ProjPath,Set},
	    State=#state{projs=Projs,status=Status}) ->
    ProjName=filename:basename(ProjPath),
    {NewProjs,NewStatus}=
	case lookup_proj(ProjName,Projs,Set) of
	    undefined ->
		%% A new file, check if it exists!
		NS=case file:read_file_info(ProjPath) of
		       {ok,_} ->
			   true;
		       _ ->
			   io:format("WARNING: ~p is missing!~n",
				     [ProjPath]),
			   false
		   end,
		Projs2=[{ProjName,ProjPath,Set}|Projs],
		{Projs2,update_status(NS,validate,Status)};
	    {ok,ProjPath} ->
		{Projs,Status};
	    {ok,OldProjPath} ->
		io:format("WARNING: Found ~p project referenced both at~n"
			  "  ~p~n"
			  "AND~n"
			  "  ~p~n",
			  [ProjName,ProjPath,OldProjPath]),
		{Projs,Status}
	end,
    {noreply, State#state{projs=NewProjs,status=NewStatus}}.


lookup_proj([],_ProjName,_Set) ->
    undefined;
lookup_proj([{ProjName,ProjPath,Set}|_],ProjName,Set) ->
    {ok,ProjPath};
lookup_proj([_|Rest],ProjName,Set) ->
    lookup_proj(Rest,ProjName,Set).


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
equal_sets(Set1,Set2,Sets) ->
    S1=proplists:get_value(Set1,Sets),
    S2=proplists:get_value(Set2,Sets),
    case diff_lists(S1,S2) of
	{[],[]} ->
	    true;
	{OnlyS1,OnlyS2} ->
	    io:format("Sets differ!~n"
		      " Only in ~p:~n~p~n"
		      " Only in ~p:~n~p~n",
		      [Set1,OnlyS1,Set2,OnlyS2]),
	    false
    end.

diff_lists(S1,S2) ->
    OnlyS1=S1--S2,
    OnlyS2=S2--S1,
    {OnlyS1,OnlyS2}.


%% Increase the value of the "Key" counter with one.
%% increase_cntr([],Key,undefined,Out) ->
%%     {1,lists:reverse([{Key,1}|Out])};
%% increase_cntr([],_Key,CntVal,Out) ->
%%     {CntVal,lists:reverse(Out)};
%% increase_cntr([{Key,Val}|Rest],Key,_CntVal,Out) ->
%%     increase_cntr(Rest,Key,Val+1,[{Key,Val+1}|Out]);
%% increase_cntr([H|Rest],Key,CntVal,Out) ->
%%     increase_cntr(Rest,Key,CntVal,[H|Out]).


    
