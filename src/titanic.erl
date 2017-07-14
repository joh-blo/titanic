%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(titanic).

%% API
-export([t/0,
	 parse_tpd/5]).

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
t() ->
    TPDfileIn="IMTAF_CSCF.tpd",
%    TPDfileIn="/home/johan/test.tpd",
    TPDfileOut="/home/johan/testOut.tpd",
    io:format("========= START PARSE =========~n",[]),
    Base="../../../../repo/imtaf/src/",
    ToolPath="/opt/DEK/IMTAF/git/cscf/ft/units/Frameworks/IMTAF_CSCF/",
    {TPD,_}=parse_tpd(TPDfileIn,initial,Base,ToolPath,[]),
    io:format("========= START EXPORT =========~n",[]),
%    io:format("========= START EXPORT =========~nTPD=~p~n",[TPD]),
    Str=titanic_xmerl:export(TPD,titanic_xmerl_xml),
%    io:format("~n~s~n",[Str]),
    file:write_file(TPDfileOut,Str).


parse_tpd(File,Mode,Base,ToolPath,TPDs) ->
    ActualFile=filename:join(ToolPath,File),
    case file:read_file(ActualFile) of
	{ok,Content} ->
	    titanic_tpd:parse(Content,Mode,Base,ToolPath,TPDs);
	Error ->
	    io:format("Cannot read ~p, got ~p~n",[File,Error]),
	    throw(Error)
    end.
