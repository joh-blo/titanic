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
-export([do/4
%	 process/2
]).

%% gen_statem callbacks
-export([callback_mode/0,init/1,terminate/3, code_change/4]).

-export(['TITAN_Project_File_Information'/3,
	 'ProjectName'/3,
	 'ReferencedProjects'/3,
	 'Folders'/3,
	 'Files'/3,
	 'ActiveConfiguration'/3,
	 'Configurations'/3
	]).

-define(SERVER, ?MODULE).
-define(HANDLE_COMMON,?FUNCTION_NAME(T,C,D) -> handle_common(T,C,D,?FUNCTION_NAME)).

-include_lib("xmerl/include/xmerl.hrl").
-include("titanic_internal.hrl").

%% User State for xmerl scanning
-record(state,{
	  opt,          % (validate|flatten|customize|external) Parsing mode
	  root,         % Absolute path to repo root in local environment
	  tpd           % #tpd{}, a complete parsed TPD
         }).


do(Root,ProjPath,Updates,Opt) ->
    case parse(Root,ProjPath,Opt) of
	Res when is_record(Res,tpd) ->
	    if
		Opt==flatten ->
		    TPD=process_tpd_flatten(Res,Updates),
		    create_xml(TPD);
		Opt==validate ->
		    Res
	    end;
	Res ->
	    Res
    end.


%% Parse a TPD file (an XML standard to control building a binary from TTCN-3)
parse(Root,ProjPath,Opt) ->
    TPDfileIn=filename:basename(ProjPath),
    TPDfileDir=filename:dirname(ProjPath),
    FilePath=filename:join([Root,ProjPath]),
    case file:read_file(FilePath) of
	{ok,Content} ->
	    do_parse(Content,Root,TPDfileDir,Opt);
	Error ->
	    io:format("Cannot read~n"
		      " ~p from~n"
		      " ~p,~n"
		      " got ~p~n",[TPDfileIn,FilePath,Error]),
	    false
    end.



do_parse(Content0,Root,TPDfileDir,Opt) ->
    Pid=start_parser(Root,TPDfileDir,Opt),
    Content=if
		is_binary(Content0) -> binary_to_list(Content0);
		is_list(Content0) -> Content0
	    end,

    {_Mode,_InCharset,C1}=xmerl_lib:detect_charset(undefined,Content),
    Event=fun(#xmerl_event{event=E,data=Data},S) when is_record(Data,xmlDecl) ->
		  if
		      E==ended -> export_element(Pid,E,unknown,Data);
		      true -> ok
		  end,
    		  S;
	     (#xmerl_event{data=Data},S) when is_record(Data,xmlAttribute) ->
		  S;
	     (#xmerl_event{event=ended,data=document},S) ->
		  S;
	     (#xmerl_event{event=Event,data=Data},S) ->
		  Name=case Data of
			   #xmlElement{name=N} ->
			       N;
			   #xmlText{parents=[{N,_}|_]} -> N;
			   #xmlComment{parents=[{N,_}|_]} -> N;
			   _ ->
			       unknown
		       end,
    		  export_element(Pid,Event,Name,Data),
    		  S
    	  end,
    xmerl_scan:string(C1,[{event_fun,Event}]),
    stop_parser(Pid).




%% Creates a single "flat" TPD, from a TPD "tree" 
process_tpd_flatten(Res=#tpd{path=TPDfileDir,
			     ref_projects=RefProjects,
			     files=Files0},Updates) ->
    Files1=process_tpd_flatten2(Files0,TPDfileDir,Updates,[]),
    {TPD,_}=process_tpd_flatten1(RefProjects,
				 Updates,
				 Res#tpd{ref_projects=[],
					 files=Files1},
				 []),
    TPD.

process_tpd_flatten1([],_Updates,TPD,AllProjs) ->
    {TPD,AllProjs};
process_tpd_flatten1([{#xmlElement{attributes=OldA,parents=Par,
				   pos=Pos,language=Lang},
		       TPD=#tpd{ref_projects=RefProjects,
				path=TPDfileDir}}|Rest],
		     Updates,
		     OrgTPD=#tpd{files=Files0},
		     AllProjs0) ->
    Name=lookup_attribute(name,OldA),
    case lists:member(Name,AllProjs0) of
	false ->
	    {#tpd{files=Files1},AllProjs1}=
		process_tpd_flatten1(RefProjects,Updates,
				     TPD#tpd{ref_projects=[]},
				     AllProjs0),

	    Comm=#xmlComment{value="From "++Name,
			     parents=Par,pos=Pos,language=Lang},
	    NewFiles=
		Files0++
		[#xmlText{value="\n"},Comm]++
		process_tpd_flatten2(Files1,TPDfileDir,Updates,[]),
	    NewTPD=OrgTPD#tpd{files=NewFiles},
	    process_tpd_flatten1(Rest,Updates,NewTPD,AllProjs1);
	true ->
	    process_tpd_flatten1(Rest,Updates,OrgTPD,AllProjs0)
    end;
process_tpd_flatten1(Other,_Updates,TPD,_) ->
    io:format("ERROR: Other=~p~n",[Other]),
    TPD.


process_tpd_flatten2([],_TPDfileDir,_Updates,Out) ->
    lists:reverse(Out);
process_tpd_flatten2([E=#xmlElement{attributes=OldA}|Rest],
		     TPDfileDir,Updates,Out) ->
    OldPath=lookup_attribute(relativeURI,OldA),
    NewPath=filename:join(TPDfileDir,OldPath),
    Path=apply_updates(Updates,NewPath),

    NewA=update_attribute(relativeURI,Path,OldA),
    NewE=E#xmlElement{attributes=NewA},
    process_tpd_flatten2(Rest,TPDfileDir,Updates,[NewE|Out]);
process_tpd_flatten2([E|Rest],TPDfileDir,Updates,Out) ->
    process_tpd_flatten2(Rest,TPDfileDir,Updates,[E|Out]).


apply_updates([],Path) ->
    Path;
apply_updates([{"",Val}|Rest],Path) ->
    NewPath=filename:join(Val,Path),
    apply_updates(Rest,NewPath);
apply_updates([{Match,Val}|Rest],Path) ->
    NewPath=filename:join(Val,string:prefix(Path,Match)),
    apply_updates(Rest,NewPath).



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
start_parser(Root,TPDfileDir,Opt) ->
    case gen_statem:start_link(?MODULE,[Root,TPDfileDir,Opt],[]) of
	{ok,Pid} ->
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
init([Root,TPDfileDir,Opt]) ->
    {ok,'TITAN_Project_File_Information',
     #state{opt=Opt,
	    root=Root,
	    tpd=#tpd{path=shorten_path(TPDfileDir)}
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
'TITAN_Project_File_Information'(cast,{next_state,Loc},State) ->
    %% Found another element inside 
    {next_state,Loc, State};
'TITAN_Project_File_Information'(cast,E,State=#state{tpd=Tpd})
  when is_record(E,xmlComment);is_record(E,xmlDecl);
       is_record(E,xmlPI);is_record(E,xmlText) ->
    %% Declarations assumed to be on the top level.
    NewTpd=Tpd#tpd{closure=[E|Tpd#tpd.closure]},
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
'TITAN_Project_File_Information'(cast,E,State=#state{tpd=Tpd})
  when is_record(E,xmlElement) ->
    NewTpd=Tpd#tpd{closure=[E#xmlElement{content=[]}|Tpd#tpd.closure]},
    {next_state,'TITAN_Project_File_Information',
     State#state{tpd=NewTpd}};
'TITAN_Project_File_Information'({call,From},stop,#state{opt=Opt,
							 tpd=Tpd}) ->
    Res=if
	    Opt==validate ->
		titanic_manager:status(validate);
	    true ->
		Tpd#tpd{ref_projects=lists:reverse(Tpd#tpd.ref_projects),
			files=lists:reverse(Tpd#tpd.files)}
	end,
    {stop_and_reply,normal,{reply,From,Res}};
?HANDLE_COMMON.



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
		     State=#state{opt=Opt,
				  tpd=Tpd=#tpd{path=TPDfileDir,
					       ref_projects=RefProjects},
				  root=Root
				 }) ->
    %% Expand the ReferencedProject
    RefTpd=expand_project(E,Root,TPDfileDir,Opt),
    NewTpd=Tpd#tpd{ref_projects=[{E,RefTpd}|RefProjects]},
    NewState=State#state{tpd=NewTpd},
    {next_state,'ReferencedProjects', NewState};
'ReferencedProjects'(cast,E=#xmlElement{name='ReferencedProjects'},State)
  when is_record(E,xmlElement) ->
    {next_state,'TITAN_Project_File_Information', State};
?HANDLE_COMMON.


'Folders'(cast,E=#xmlElement{name='Folders'},State)
  when is_record(E,xmlElement) ->
    {next_state,'TITAN_Project_File_Information', State};
'Folders'(cast,_E,State) ->
    {next_state,'Folders', State};
?HANDLE_COMMON.



%% Files is a container element, holding a set of FileResource elements and
%% nothing else.
'Files'(cast,E=#xmlElement{name='FileResource'},
	State=#state{tpd=Tpd=#tpd{name=TpdName,
				  path=TPDfileDir,
				  files=Files},
		     root=Root
		    }) ->
    check_file(E,TpdName,Root,TPDfileDir),
    NewTpd=Tpd#tpd{files=[E|Files]},
    {next_state,'Files', State#state{tpd=NewTpd}};
'Files'(cast,E,State)
  when is_record(E,xmlElement) ->
    {next_state,'TITAN_Project_File_Information', State};
'Files'(cast,E,State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{files=[E|Tpd#tpd.files]},
    {next_state,'Files', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


%% Anything else in the 'TITAN_Project_File_Information' element
'ProjectName'(cast,E=#xmlElement{name='ProjectName'},State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{name=E},
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


'ActiveConfiguration'(cast,E=#xmlElement{name='ActiveConfiguration'},
		      State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{active_config=E},
    {next_state,'TITAN_Project_File_Information', State#state{tpd=NewTpd}};
?HANDLE_COMMON.


'Configurations'(cast,E=#xmlElement{name='Configuration'},
		 State=#state{tpd=Tpd}) ->
    NewTpd=Tpd#tpd{configs=[E|Tpd#tpd.configs]},
    {next_state,'Configurations', State#state{tpd=NewTpd}};
'Configurations'(cast,#xmlElement{name='Configurations'},State) ->
    {next_state,'TITAN_Project_File_Information', State};
?HANDLE_COMMON.


handle_common(cast,_,S,L) ->
    %% Ignore anything that does not need any special handling.
    {next_state,L,S};
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
	true ->
	    ok
    end,
    if
	E==started,is_record(ParsedEntity,xmlElement) ->
	    gen_statem:cast(Pid,{next_state,Name});
	E==ended->
	    gen_statem:cast(Pid,ParsedEntity);
	true ->
	    ok
    end.

stop_parser(Pid) ->
    gen_statem:call(Pid,stop,infinity).


%%% ============================================================================
%% Update paths etc. in the elements

check_file(#xmlElement{attributes=OldA},TpdName,Root,TPDfileDir) ->
    OldPath=lookup_attribute(relativeURI,OldA),
    FilePath=shorten_path(filename:join([Root,TPDfileDir,OldPath])),
    ProjName=projname(TpdName),
    titanic_manager:check_file(FilePath,ProjName).


expand_project(#xmlElement{attributes=OldA},Root,TPDfileDir,Opt) ->
    RelativeProjPath=lookup_attribute(projectLocationURI,OldA),
    ProjPath=filename:join([TPDfileDir,RelativeProjPath]),
    FilePath=shorten_path(filename:join([Root,ProjPath])),
    titanic_manager:check_project(FilePath),
    parse(Root,ProjPath,Opt).



%%% ----------------------------------------------------------------------------
%% Creates a TPD file (XML file) from a #tpd{} record
create_xml(TPD=#tpd{closure=Closure}) ->
    L=create_xml_closure(Closure,TPD,[]),
    titanic_xmerl:export(L,titanic_xmerl_xml).

%% Note that the closure list is in reverse order since parsing 
create_xml_closure([],_TPD,Out) ->
    Out;
create_xml_closure([E=#xmlElement{name='TITAN_Project_File_Information',
				  content=_Content}|Rest],
		   TPD=#tpd{name=Name,
			    ref_projects=RefProjects,
			    files=FileRes,
			    active_config=ActiveConfig,
			    configs=Configs0
			   },Out) ->
    RefProj=if
		RefProjects==[] ->
		    [];
		true ->
		    [#xmlText{value="\n"},
		     #xmlElement{name='ReferencedProjects',
				 content=RefProjects}]
	    end,
    Files=if
	      FileRes==[] ->
		  [];
	      true ->
		  [#xmlText{value="\n"},
		   #xmlElement{name='Files',
			       content=FileRes}]
	  end,
    ActConf=if
		ActiveConfig==[] ->
		    [];
		true ->
		    [#xmlText{value="\n"},ActiveConfig]
	    end,
    Configs=if
     		Configs0==[] ->
     		    Configs0;
     		true ->
     		    [#xmlText{value="\n"}|Configs0]
     	    end,

    NewContent=[#xmlText{value="\n"},Name]++
	RefProj++
	Files++
	ActConf++
	Configs++[#xmlText{value="\n"}],
    NewE=E#xmlElement{content=NewContent},
    create_xml_closure(Rest,TPD,[NewE|Out]);
create_xml_closure([H|Rest],TPD,Out) ->
    create_xml_closure(Rest,TPD,[H|Out]).


%%% ----------------------------------------------------------------------------
%% Utillities

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


projname(#xmlElement{name='ProjectName',
		     content=[#xmlText{value=Content}]}) ->
    Content.



%% Remove any ".." in a path, if possible.
shorten_path(Path) ->
    shorten_path2(filename:split(Path),[]).

shorten_path2([],Out) ->
    filename:join(lists:reverse(Out));
shorten_path2([".."|Rest],[_|Out]) ->
    shorten_path2(Rest,Out);
shorten_path2([H|Rest],Out) ->
    shorten_path2(Rest,[H|Out]).

