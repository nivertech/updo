-module(updo_low).

-export([translate_and_run/1,
         translate_and_check/1]).

%%%
%%% Generate and execute low level upgrade scripts from the instruction set
%%% of relup files.
%%%

-include_lib("sasl/src/systools.hrl").


translate_and_run(HLinstrs) ->
    {Apps, LLinstrs} = translate(HLinstrs, loaded_apps()),
    LibDirs = app_lib_dirs(Apps),
    check_script(LLinstrs, LibDirs),
    % The interface to release_handler_1 changed in R15.
    code:ensure_loaded(release_handler_1),
    case erlang:function_exported(release_handler_1, eval_script, 5) of
        true ->
            % R15
            release_handler_1:eval_script(LLinstrs, [], LibDirs, LibDirs, []);
        false ->
            % R14 or older
            release_handler_1:eval_script(LLinstrs, [], LibDirs)
    end.

translate_and_check(HLinstrs) ->
    {Apps, LLinstrs} = translate(HLinstrs, loaded_apps()),
    LibDirs = app_lib_dirs(Apps),
    check_script(LLinstrs, LibDirs),
    {ok, LLinstrs}.


%%% ========================================================================
%%% Internal
%%% ========================================================================

translate(HLinstrs, AppsNow) ->
    AppsAfter = apps_after(HLinstrs, AppsNow),
    AppsNowRecs = app_records(AppsNow),
    AppAfterRecs = app_records(AppsAfter),
    case systools_rc:translate_scripts([HLinstrs], AppAfterRecs, AppsNowRecs) of
	{ok, LLinstrs} ->
            {AppsAfter, LLinstrs};
	{error, systools_rc, Error} ->
	    io:format(systools_rc:format_error(Error)),
	    exit(parse_error)
    end.

check_script(LLinstrs, LibDirs) ->
    case release_handler_1:check_script(LLinstrs, LibDirs) of
        ok ->
            ok;
        {ok, _} ->
            ok;
        {error, Error} ->
            exit(Error)
    end.

apps_after(HLinstrs, Before) ->
    Added = [ Name || {add_application, Name} <- HLinstrs ],
    Removed = [ Name || {remove_application, Name} <- HLinstrs ],
    lists:usort(Before ++ Added) -- Removed.

loaded_apps() ->
    [ App || {App, _, _} <- application:loaded_applications() ].

app_records(Apps) ->
    [ #application{name = A, modules = find_app_modules(A)} || A <- Apps ].

find_app_modules(App) ->
    Ext = code:objfile_extension(),
    case code:lib_dir(App, ebin) of
	Path when is_list(Path) ->
	    Files = filelib:wildcard("*" ++ Ext, Path),
	    [ list_to_atom(filename:basename(F, Ext)) || F <- Files ];
	{error, _} ->
	    io:format("Can't find lib dir for application '~s'", [App]),
	    exit({unknown_application, App})
    end.

app_lib_dirs(Apps) ->
    [ {App, "", code:lib_dir(App)} || App <- Apps ].

