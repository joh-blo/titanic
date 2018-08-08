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
    TPD=parse_tpd(TPDfileIn,initial,LibRoot,ToolPath),
%    io:format("========= START EXPORT =========~n",[]),
    io:format("========= START EXPORT =========~nTPD=~p~n",[TPD]),
    Str=titanic_xmerl:export(TPD,titanic_xmerl_xml),
%    io:format("~n~s~n",[Str]),
    file:write_file(TPDfileOut,Str).


parse_tpd(File,Mode,LibRoot,ToolPath) ->
    ActualFile=filename:join(ToolPath,File),
    case file:read_file(ActualFile) of
	{ok,Content} ->
	    titanic_tpd:parse(Content,Mode,LibRoot,ToolPath);
	Error ->
	    io:format("Cannot read ~p, got ~p~n",[File,Error]),
	    throw(Error)
    end.
