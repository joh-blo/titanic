%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%% @end
%%% Created :  5 Jul 2017 by Johan <>
%%%-------------------------------------------------------------------
%% erl -pa titanic/ebin/
-module(titanic).

%% API
-export([t/0,t/2,
	 parse_tpd/4]).

-include_lib("xmerl/include/xmerl.hrl").
-include("titanic_internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% "/opt/DEK/IMTAF/git/"
t() ->
    t("/opt/DEK/IMTAF/git/",test).

t(Root,Target) ->
%    Base="../../../../repo/imtaf/src/",
    LibRoot="../../../../opt/DEK/IMTAF/git/imtaf/src/",
    {TPDfileIn,ToolPath}=
	case Target of
	    test ->
		{"InTest.tpd",
		 filename:join(Root,"")};
	    cscf ->
		{"IMTAF_CSCF.tpd",
		 filename:join(Root,"cscf/ft/units/Frameworks/IMTAF_CSCF/")};
	    hss ->
		{"IMTAF_HSS.tpd",
		 filename:join(Root,"HSS/ft/units/IMTAFFrameworkLibs/IMTAF/")}
	end,
    TPDfileOut=filename:join(Root,"testOut.tpd"),
    io:format("========= START PARSE =========~n",[]),
    TPD0=parse_tpd(TPDfileIn,initial,LibRoot,ToolPath),
    io:format("========= START PROCESSING =========~n",[]),
    TPD1=process_tpd(TPD0,flatten),

    io:format("========= START EXPORT =========~n",[]),
    Str=create_xml(TPD1),
%    io:format("~n~s~n",[Str]),
    file:write_file(TPDfileOut,Str).

%% Parse a TPD file (an XML standard to control building a binary from TTCN-3)
parse_tpd(File,Mode,LibRoot,ToolPath) ->
    ActualFile=filename:join(ToolPath,File),
    case file:read_file(ActualFile) of
	{ok,Content} ->
	    A=titanic_tpd:parse(Content,Mode,LibRoot,ToolPath),
	    io:format("parse_tpd in ~p~n |Tpd.files|=~p~n",
		      [self(),length(A#tpd.files)]),
	    A;
	Error ->
	    io:format("Cannot read ~p, got ~p~n",[File,Error]),
	    throw(Error)
    end.

%% Process the parsed TPD, or tree of TPDs and perform some action.
%% Supported processing options include:
%% + validate- Parse and validate 
%% + flatten - Flattens a tree with TPDs by merging all referenced projects and
%%             and remove duplicates.
process_tpd(TPD=#tpd{ref_projects=RefProjects},validate) ->
    NewRefProjects=[E || {E,_R} <- RefProjects],
    TPD#tpd{ref_projects=NewRefProjects};
process_tpd(TPD,Options) ->
    titanic_tpd:process(TPD,Options).




%% Creates a TPD file (XML file) from a #tpd{} record
create_xml(TPD=#tpd{closure=Closure}) ->
    io:format(" Closure=~p~n",[Closure]),
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
			    configs=Configs
			   },Out) ->
    io:format(" Name=~p~n"
	      " RefProjects=~p~n"
%	      " Files=~p~n"
	      " ActiveConfig=~p~n"
	      " Configs=~p~n",
	      [Name,RefProjects,
%	       Files,
	       ActiveConfig,Configs]),
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
	
    NewContent=[#xmlText{value="\n"},Name]++
	RefProj++
	Files++
	ActConf++
	Configs++[#xmlText{value="\n"}],
    NewE=E#xmlElement{content=NewContent},
    create_xml_closure(Rest,TPD,[NewE|Out]);
create_xml_closure([H|Rest],TPD,Out) ->
    create_xml_closure(Rest,TPD,[H|Out]).
    
