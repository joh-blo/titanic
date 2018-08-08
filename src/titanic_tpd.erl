%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(titanic_tpd).

-behaviour(gen_statem).

%% API
-export([parse/4]).

%% gen_statem callbacks
-export([callback_mode/0,init/1,terminate/3, code_change/4]).

-export(['TITAN_Project_File_Information'/3,
	 'ProjectName'/3,
	 'ReferencedProjects'/3,
	 'Files'/3,
	 'ActiveConfiguration'/3,
	 'Configurations'/3
	]).

-define(SERVER, ?MODULE).
-define(HANDLE_COMMON,?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D,?FUNCTION_NAME)).

-include_lib("xmerl/include/xmerl.hrl").

%% User State for xmerl scanning
-record(state,{
	  mode,     % (initial | expanding | off) Parsing mode
	  base,     % Base path for the TPD
	  tool_path,% Relative path from tool
	  tpd       % #tpd{}, a complete parsed TPD
         }).

%% TPD structure
-record(tpd,{
	 name="",
	 closure=[],
	 ref_projects=[],
	 files=[],
	 active_config="",
	 configs=[]
	}).


parse(Content0,Mode,Base,ToolPath) ->
    Pid=start_parser(Mode,Base,ToolPath),
    io:format("JB STARTING PARSER ~p~n",[Pid]),
    Content=if
		is_binary(Content0) -> binary_to_list(Content0);
		is_list(Content0) -> Content0
	    end,

    {_Mode,_InCharset,C1}=xmerl_lib:detect_charset(undefined,Content),
%    io:format("~p:parse _Mode=~p~n _InCharset=~p~n",[?MODULE,_Mode,_InCharset]),
    %% Hook=fun(ParsedEntity, S) ->
    %% 		 export_element(Pid,ParsedEntity),
    %% 		 %% if
    %% 		 %%     is_record(ParsedEntity,xmlText) ->
    %% 		 %% 	 ok;
    %% 		 %%     true ->
    %% 		 %% 	 export_element(Pid,ParsedEntity)
    %% 		 %% end,
    %% 		 {ParsedEntity,S}
    %% 	 end,
    Event=fun(#xmerl_event{event=E,data=Data},S) when is_record(Data,xmlDecl) ->
		  if
		      E==ended -> export_element(Pid,E,unknown,Data);
		      true -> ok
		  end,
    		  S;
	     (#xmerl_event{data=Data},S) when is_record(Data,xmlAttribute) ->
		  S;
	     (X=#xmerl_event{event=ended,data=document},S) ->
		  io:format("Document ended ~p~n",[X]),
		  S;
	     (EV=#xmerl_event{event=Event,data=Data},S) ->
		  Name=case Data of
			   #xmlElement{name=N} ->
%			       io:format("Event=~p~n",[EV]),
			       N;
			   #xmlText{parents=[{N,_}|_]} -> N;
			   #xmlComment{parents=[{N,_}|_]} -> N;
			   _ ->
			       io:format("Event=~p~n",[EV]),
			       unknown
		       end,
    		  export_element(Pid,Event,Name,Data),
    		  S
	     %% (#xmerl_event{event=Event,data=Data},S) ->
	     %% 	  io:format("Event=~p~n Data=~p~n",[Event,Data]),
    	     %% 	  S
    	  end,
    xmerl_scan:string(C1,[{event_fun,Event}]),
    io:format("JB STOPPING PARSER ~p~n",[Pid]),
    stop_parser(Pid).




%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_parser(Mode,Base,ToolPath) ->
    case gen_statem:start_link(?MODULE,[Mode,Base,ToolPath],[]) of
	{ok,Pid} ->
	    io:format("start_parser ~p started!~n",[Pid]),
	    Pid;
	{already_started,Pid} ->
	    io:format("start_parser ~p already started!~n",[Pid]),
	    Pid;
	Error ->
	    io:format("start_parser Eror:~p~n",[Error]),
	    Error
    end.

%%%===================================================================
%%% gen_statem callbacks

%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
-spec init(Args :: term()) ->
		  gen_statem:init_result(atom()).
init([Mode,Base,ToolPath]) ->
    {ok,'TITAN_Project_File_Information',
     #state{mode=Mode,
	    base=Base,
	    tool_path=ToolPath,
	    tpd=#tpd{}
	   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Apply all collected updates here...
%% @end
-spec 'TITAN_Project_File_Information'('enter',
	   OldState :: atom(),
	   Data :: term()) ->
		  gen_statem:state_enter_result('state_name');
	  (gen_statem:event_type(),
	   Msg :: term(),
	   Data :: term()) ->
		  gen_statem:event_handler_result(atom()).
'TITAN_Project_File_Information'(cast,_,State=#state{mode=off}) ->
    %% Just wait for parser to stop
    {next_state,'TITAN_Project_File_Information', State};
'TITAN_Project_File_Information'(cast,{next_state,Loc},State) ->
    %% Found another element inside 
    io:format("'TITAN_Project_File_Information' ==> ~p~n",[Loc]),
    {next_state,Loc, State};
'TITAN_Project_File_Information'(cast,E,State=#state{mode=initial,tpd=Tpd})
  when is_record(E,xmlComment);is_record(E,xmlDecl);is_record(E,xmlPI) ->
    %% Declarations assumed to be on the top level.
    NewTpd=Tpd#tpd{closure=[E|Tpd#tpd.closure]},
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
'TITAN_Project_File_Information'(cast,E,State=#state{tpd=Tpd,
						     mode=initial})
  when is_record(E,xmlElement) ->
%    Content=lookup_cache('TITAN_Project_File_Information',Cache),
%    NewE=E#xmlElement{content=Content},
    NewTpd=Tpd,
    {next_state,'TITAN_Project_File_Information',
     State#state{tpd=NewTpd}};
'TITAN_Project_File_Information'(cast,_,State) ->
    %% Ignore anything else as it does not need any special handling.
    {next_state,'TITAN_Project_File_Information', State};
'TITAN_Project_File_Information'({call,From},stop,#state{tpd=Tpd}) ->
    io:format("'TITAN_Project_File_Information' Returning from ~p~n |Tpd.files|=~p~n",
	      [self(),length(Tpd#tpd.files)]),
    {stop_and_reply,normal,{reply,From,Tpd}}.



%% ReferencedProjects is a container element, holding a set of ReferencedProject
%% that only occurs in the root element.
%% A 'ReferencedProject' always point to another TPD file and only occurs
%% in the ReferencedProjects element. If expanding, merge the content of
%% the TPD file.
%% Note:
%% - The 'ReferencedProjects' elements should always be expanded and removed,
%%   thus not stored explicitly.
%% - All relative paths must be copied and extended, in all references.
'ReferencedProjects'(cast,E=#xmlElement{name='ReferencedProject'},
		     State=#state{tpd=Tpd,base=Base,mode=Mode,
				  tool_path=ToolPath}) ->
    %% Expand and merge the ReferencedProject!
    RefTpd=expand_element(projectLocationURI,Base,E,ToolPath),
    NewState=
	if
	    Mode==expanding ->
%		io:format("E=~p~n Elist=~p~n",[E,Elist]),
		NewTpd=Tpd#tpd{ref_projects=[RefTpd|Tpd#tpd.ref_projects]},
		State#state{tpd=NewTpd};
	    true ->
		State
	end,
    {next_state,'ReferencedProjects', NewState};
'ReferencedProjects'(cast,E,State)
  when is_record(E,xmlElement) ->
    io:format("DONE ~p with ReferencedProject: ~n Tpd=~p~n",[self(),State#state.tpd]),
    io:format("'ReferencedProjects' ==> 'TITAN_Project_File_Information'~n",[]),
    {next_state,'TITAN_Project_File_Information', State};
'ReferencedProjects'(cast,E,State=#state{}) ->
    %% Ignore text and comments within this element
%    io:format("CAST: 'ReferencedProjects' E=~p~n",[E]),
%    NewCache=update_cache('Files',[E],Cache),
    {next_state,'ReferencedProjects', State};
?HANDLE_COMMON.


%% Files is a container element, holding a set of FileResource elements and
%% nothing else.
'Files'(cast,E=#xmlElement{name='FileResource'},
	       State=#state{tpd=Tpd,base=Base,mode=Mode}) ->
    %% FIXME! Expand any paths in FileResource!
    NewE=update_path_in_element(relativeURI,Base,E),
%    io:format("EXPAND OLD:~p~n  NEW:~p~n",[E,NewE]),
%    NewCache=update_cache('Files',[NewE],Cache),
    NewTpd=if
		Mode==initial ->
		    Tpd;
		Mode==expanding ->
		    Tpd#tpd{files=[NewE|Tpd#tpd.files]}
	    end,
    {next_state,'Files', State#state{tpd=NewTpd}};
'Files'(cast,E,State)
  when is_record(E,xmlElement) ->
    %% Content=lookup_cache('Files',Cache),
    %% NewE=E#xmlElement{content=Content},
    %% NewCache=update_cache('TITAN_Project_File_Information',[NewE],Cache),
    io:format("'Files' ==> 'TITAN_Project_File_Information'~n",[]),
    {next_state,'TITAN_Project_File_Information', State};
'Files'(cast,E,State=#state{tpd=Tpd}) ->
    %% NewCache=update_cache('Files',[E],Cache),
    NewTpd=Tpd#tpd{files=[E|Tpd#tpd.files]},
    {next_state,'Files', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


%% Anything else in the 'TITAN_Project_File_Information' element
'ProjectName'(cast,E,State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{name=E},
    io:format("'ProjectName' ==> 'TITAN_Project_File_Information'~n",[]),
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


'ActiveConfiguration'(cast,E,State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{active_config=E},
    io:format("'ActiveConfiguration' ==> 'TITAN_Project_File_Information'~n",[]),
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


'Configurations'(cast,E=#xmlElement{name='Configurations'},
		 State=#state{tpd=Tpd}) ->
%    io:format("Configurations-1 ~n E=~p~n S=~p~n",[E, State]),
    NewTpd=Tpd#tpd{configs=E},
    io:format("'Configurations' ==> 'TITAN_Project_File_Information'~n",[]),
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
'Configurations'(cast,E,State) ->
    {next_state,'Configurations', State};
?HANDLE_COMMON.


handle_common(T,E,S,L) ->
    io:format("handle_common~n T=~p~n E=~p~n S=~p~n L=~p~n",[T,E,S,L]),
    {keep_state,S,[postpone]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
		       any().
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
-spec code_change(
	OldVsn :: term() | {down,term()},
	State :: term(), Data :: term(), Extra :: term()) ->
			 {ok, NewState :: term(), NewData :: term()} |
			 (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



%%% ============================================================================
%% Call back process used to handle exported elements

export_element(Pid,E,Name,ParsedEntity) ->
    if
	is_record(ParsedEntity,xmlDecl);
	is_record(ParsedEntity,xmlComment) ->
	    ok;
	Name==unknown ->
      io:format("export_element~n E=~p~n ParsedEntity=~p~n",[E,ParsedEntity]);
	true ->
	    ok
    end,
    if
	E==started,is_record(ParsedEntity,xmlElement) ->
%	    io:format("export_element IN ~p:~p:~p~n",[Pid,Name,E]),
	    gen_statem:cast(Pid,{next_state,Name});
	E==ended->
	    gen_statem:cast(Pid,ParsedEntity);
	true ->
	    ok
    end.

stop_parser(Pid) ->
    A=gen_statem:call(Pid,stop,infinity),
    io:format("stop_parser in ~p, got from ~p~n |Tpd.files|=~p~n",
	      [Pid,self(),length(A#tpd.files)]),

    A.


%%% ============================================================================
%% Here goes the handling of all elements

%%% ----------------------------------------------------------------------------
%% lookup_cache(Name,Cache) ->
%%     proplists:get_value(Name,Cache,[]).

%% update_cache(Name,ExpandedE,Cache) ->
%%     case proplists:get_value(Name,Cache) of
%% 	undefined ->
%% 	    [{Name,ExpandedE}|Cache];
%% 	Content ->
%% 	    lists:keyreplace(Name,1,Cache,{Name,Content++ExpandedE})
%%     end.


update_path_in_element(AttrName,Base,E=#xmlElement{attributes=OldA}) ->
    OldPath=lookup_attribute(AttrName,OldA),
    NewPath=filename:join(Base,OldPath),
%    io:format("~p + ~p = ~p~n",[Base,OldPath,NewPath]),
    NewA=update_attribute(AttrName,NewPath,OldA),
    E#xmlElement{attributes=NewA}.

expand_element(AttrName,Base,E=#xmlElement{attributes=OldA,
					   parents=Par,pos=Pos,language=Lang},
	       ToolPath) ->
    %% We should have a coresponding .file file here as well
    OldPath=lookup_attribute(AttrName,OldA),
    Name=lookup_attribute(name,OldA),
    NewPath=filename:join(Base,OldPath),
    NewBase=filename:dirname(NewPath),
    RefTpd=titanic:parse_tpd(NewPath,expanding,NewBase,ToolPath),
    Comm=#xmlComment{value="From "++Name,
		     parents=Par,pos=Pos,language=Lang},
    RefTpd.



%%% Lookup attributes in Attribute list returned by xmerl.
lookup_attribute(K,[#xmlAttribute{name=K,value=V}|Attrs]) ->
    V;
lookup_attribute(K,[_|Attrs]) ->
    lookup_attribute(K,Attrs);
lookup_attribute(_,[]) ->
    {error,attribute_not_found}.


update_attribute(K,NewV,Attrs) ->
    case lists:keysearch(K,#xmlAttribute.name,Attrs) of
	{value,A} ->
	    NewA=A#xmlAttribute{value=NewV},
	    lists:keyreplace(K,#xmlAttribute.name,Attrs,NewA)
    end.

