-module(titanic_tpd).
-behaviour(gen_server).

-export([parse/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("xmerl/include/xmerl.hrl").

%% User State for xmerl scanning
-record(state,{
	  mode,    % (header | body | off) Parsing mode
	  base,    % Base path for the TPD
	  tool_path, % Relative path from tool
	  cache,   % Key value list with parsed elements, not yet 
	  data,    % Complete parsed response PDU
	  tpds     % List with covered TPDs
         }).

parse(Content0,Mode,Base,ToolPath,TPDs) ->
    Pid=start_parser(Mode,Base,ToolPath,TPDs),
    Content=if
		is_binary(Content0) -> binary_to_list(Content0);
		is_list(Content0) -> Content0
	    end,
    {_Mode,_InCharset,C1}=xmerl_lib:detect_charset(undefined,Content),
%    io:format("~p:parse _Mode=~p~n _InCharset=~p~n",[?MODULE,_Mode,_InCharset]),
    Hook=fun(ParsedEntity, S) ->
		 export_element(Pid,ParsedEntity),
		 %% if
		 %%     is_record(ParsedEntity,xmlText) ->
		 %% 	 ok;
		 %%     true ->
		 %% 	 export_element(Pid,ParsedEntity)
		 %% end,
		 {ParsedEntity,S}
	 end,
    Event=fun(#xmerl_event{event=ended,
    			   data=Data},S) when is_record(Data,xmlDecl) ->
    		  export_element(Pid,Data),
    		  S;
    	     (X,S) ->
    		  S
    	  end,
    xmerl_scan:string(C1,[{hook_fun,Hook},{event_fun,Event}]),
    stop_parser(Pid).



%%% ============================================================================
%% Call back process used to handle exported elements

export_element(Pid,ParsedEntity) ->
%    io:format("export_element ~p~n",[ParsedEntity]),
    gen_server:cast(Pid,ParsedEntity).



start_parser(Mode,Base,ToolPath,TPDs) ->
    case gen_server:start_link(?MODULE,[Mode,Base,ToolPath,TPDs],[]) of
	{ok,Pid} -> Pid;
	{already_started,Pid} -> Pid;
	Error -> Error
    end.

init([Mode,Base,ToolPath,TPDs]) ->
    {ok,#state{mode=Mode,
	       base=Base,
	       tool_path=ToolPath,
	       data=[],
	       cache=[],
	       tpds=TPDs
	       }}.

stop_parser(Pid) ->
    gen_server:call(Pid,stop,infinity).

terminate(_Reason, _State) ->
    ok.


handle_cast(_,State=#state{mode=off}) ->
    %% Just wait for parser to stop
    {noreply, State};

%% The root element, apply all collected updates here...
handle_cast(E=#xmlElement{name=N='TITAN_Project_File_Information'},
	    State=#state{data=Data,cache=Cache,mode=initial}) ->
    Content=lookup_cache(N,Cache),
    NewE=E#xmlElement{content=Content},
    NewData=[NewE|Data],
    {noreply, State#state{data=NewData,cache=[]}};
handle_cast(E=#xmlText{parents=[{N='TITAN_Project_File_Information',2}]},
	    State=#state{cache=Cache}) ->
    NewCache=update_cache(N,[E],Cache),
    {noreply, State#state{cache=NewCache}};
handle_cast(E=#xmlComment{parents=[{N='TITAN_Project_File_Information',2}]},
	    State=#state{cache=Cache}) ->
    NewCache=update_cache(N,[E],Cache),
    {noreply, State#state{cache=NewCache}};

%% The Files element needs special treatment, as we need extend all given paths
%% with the Base path.
handle_cast(E=#xmlElement{name=N='Files',
			  parents=[{N1='TITAN_Project_File_Information',2}]},
	    State=#state{cache=Cache}) ->
    Content=lookup_cache(N,Cache),
    NewE=E#xmlElement{content=Content},
    NewCache=update_cache(N1,[NewE],Cache),
    {noreply, State#state{cache=NewCache}};
handle_cast(E=#xmlElement{name='FileResource',
			  attributes=OldA,
			  parents=[{N='Files',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{cache=Cache,data=Data,base=Base,mode=Mode}) ->
    %% FIXME! Expand any paths in FileResource!
    NewE=update_path_in_element(relativeURI,Base,E),
%    io:format("EXPAND OLD:~p~n  NEW:~p~n",[E,NewE]),
    NewCache=update_cache(N,[NewE],Cache),
    NewData=if
		Mode==initial -> Data;
		Mode==expanding -> [NewE|Data]
	    end,
    {noreply, State#state{data=NewData,cache=NewCache}};
handle_cast(E=#xmlElement{parents=[{N='Files',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{cache=Cache}) ->
    %% Anything else in the Files element
    NewCache=update_cache(N,[E],Cache),
    {noreply, State#state{cache=NewCache}};
handle_cast(E=#xmlText{parents=[{N='Files',_},
				{'TITAN_Project_File_Information',_}
			       ]},
	    State=#state{cache=Cache,data=Data,mode=Mode}) ->
    NewCache=update_cache(N,[E],Cache),
    NewData=if
		Mode==initial -> Data;
		Mode==expanding -> [E|Data]
	    end,
    {noreply, State#state{data=NewData,cache=NewCache}};
handle_cast(E=#xmlComment{parents=[{N='Files',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{cache=Cache,data=Data,mode=Mode}) ->
    NewCache=update_cache(N,[E],Cache),
    NewData=if
		Mode==initial -> Data;
		Mode==expanding -> [E|Data]
	    end,
    {noreply, State#state{data=NewData,cache=NewCache}};



%% The ReferencedProjects element needs special treatment, as we need to expand
%% "copy in" and extend all paths in all given references.
%% Note:
%% - A 'ReferencedProject' always point to another TPD file
%% - The 'ReferencedProjects' elements should always be removed, thus
%%   not store it in the cache!
handle_cast(E=#xmlElement{name=N='ReferencedProjects',
			  parents=[{N1='TITAN_Project_File_Information',_}]},
	    State) ->
    {noreply, State};
handle_cast(E=#xmlElement{name='ReferencedProject',
			  parents=[{'ReferencedProjects',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{data=Data,cache=Cache,base=Base,mode=Mode,
			 tool_path=ToolPath,tpds=InTPDs}) ->
    %% FIXME! Expand any ReferencedProject!
    {Elist,OutTPDs}=expand_element(projectLocationURI,Base,E,ToolPath,InTPDs),
    NewState=
	if
	    Mode==expanding ->
		io:format("E=~p~n Elist=~p~n",[E,Elist]),
		NewData=lists:reverse(Elist)++Data,
		State#state{data=NewData,tpds=OutTPDs};
	    true ->
		NewCache=update_cache('Files',Elist,Cache),
		State#state{cache=NewCache,tpds=OutTPDs}
	end,
    {noreply, NewState};
handle_cast(E=#xmlElement{parents=[{'ReferencedProjects',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{cache=Cache}) ->
    %% Anything else in the ReferencedProjects element
    NewCache=update_cache('Files',[E],Cache),
    {noreply, State#state{cache=NewCache}};
handle_cast(E=#xmlText{parents=[{'ReferencedProjects',_},
				{'TITAN_Project_File_Information',_}
			       ]},
	    State=#state{cache=Cache}) ->
    NewCache=update_cache('Files',[E],Cache),
    {noreply, State#state{cache=NewCache}};
handle_cast(E=#xmlComment{parents=[{'ReferencedProjects',_},
				   {'TITAN_Project_File_Information',_}
				  ]},
	    State=#state{cache=Cache}) ->
    NewCache=update_cache('Files',[E],Cache),
    {noreply, State#state{cache=NewCache}};


%% Anything else in the root element
handle_cast(E=#xmlElement{parents=[{N='TITAN_Project_File_Information',2}]},
	    State=#state{cache=Cache}) ->
    NewCache=update_cache(N,[E],Cache),
    {noreply, State#state{cache=NewCache}};


handle_cast(M,State=#state{mode=initial,
			   data=Data}) when is_record(M,xmlComment);
					    is_record(M,xmlDecl);
					    is_record(M,xmlPI) ->
    %% Declarations assumed to be on the top level.
    {noreply, State#state{data=[M|Data]}};

%% Ignore anything else as it does not need any special handling.
handle_cast(_,State) ->
    {noreply, State}.


handle_call(stop,_From,State=#state{data=Data,tpds=TPDs}) ->
    {stop,normal,{lists:reverse(Data),TPDs},State}.


handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%% Here goes the handling of all elements

%%% ----------------------------------------------------------------------------
lookup_cache(Name,Cache) ->
    proplists:get_value(Name,Cache,[]).

update_cache(Name,ExpandedE,Cache) ->
    case proplists:get_value(Name,Cache) of
	undefined ->
	    [{Name,ExpandedE}|Cache];
	Content ->
	    lists:keyreplace(Name,1,Cache,{Name,Content++ExpandedE})
    end.


update_path_in_element(AttrName,Base,E=#xmlElement{attributes=OldA}) ->
    OldPath=lookup_attribute(AttrName,OldA),
    NewPath=filename:join(Base,OldPath),
%    io:format("~p + ~p = ~p~n",[Base,OldPath,NewPath]),
    NewA=update_attribute(AttrName,NewPath,OldA),
    E#xmlElement{attributes=NewA}.

expand_element(AttrName,Base,E=#xmlElement{attributes=OldA,
					   parents=Par,pos=Pos,language=Lang},
	       ToolPath,InTPDs) ->
    OldPath=lookup_attribute(AttrName,OldA),
    Name=lookup_attribute(name,OldA),
    TPD=filename:basename(OldPath),
    case lists:member(TPD,InTPDs) of
	true -> %% Ignore expansion
	    Comm=#xmlComment{value="Skipping "++Name,
			     parents=Par,pos=Pos,language=Lang},
	    {[Comm],InTPDs};
	false ->
	    NewPath=filename:join(Base,OldPath),
	    NewBase=filename:dirname(NewPath),
	    Comm=#xmlComment{value="From "++Name,
			     parents=Par,pos=Pos,language=Lang},
	    {Elements,OutTPDs}=titanic:parse_tpd(NewPath,expanding,NewBase,
						 ToolPath,[TPD|InTPDs]),
	    {[Comm|Elements],OutTPDs}
    end.



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

