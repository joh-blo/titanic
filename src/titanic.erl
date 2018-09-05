%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%% @end
%%% Created :  5 Jul 2017 by Johan <>
%%%-------------------------------------------------------------------
%% erl -pa titanic/ebin/ -run titanic
-module(titanic).

%% API
-export([start/0,
	 validate/2,
	 flatten/2,flatten/3,
	 external/4]).

%%%===================================================================
%%% API
%%%===================================================================


start() ->
    application:start(?MODULE).



%% validate- Parse and validate.
%% Validates that all referenced projects and files in these actually exists.
validate(GitRoot,ProjPath) ->
    titanic_manager:reset(),
    titanic_tpd:do(GitRoot,ProjPath,[],validate).
    
%% flatten - Flattens a tree with TPDs.
%% + Search all referenced files and update all file paths according to the
%%   key/value matching list in Updates.
%% + Merge all referenced projects while removing duplicates and store in
%%   a single TPD file. Includes a validate/2
flatten(GitRoot,ProjPath) ->
    flatten(GitRoot,ProjPath,[]).

flatten(GitRoot,ProjPath,Updates) ->
    TPDfileOut=filename:join("/tmp","FlatOut.tpd"),
    titanic_manager:reset(),
    Str=titanic_tpd:do(GitRoot,ProjPath,Updates,flatten),
    file:write_file(TPDfileOut,Str).


%% + external - Hook for external, user defined actions defined in a callback
%% module. Includes a validate/2
external(GitRoot,ProjPath,Updates,CB) ->
    titanic_manager:reset(),
    titanic_tpd:do(GitRoot,ProjPath,Updates,{external,CB}).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
