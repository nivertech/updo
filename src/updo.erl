-module(updo).

-export([run/0,
         run/1,
         dry_run/0,
         dry_run/1,
         dry_run_low/0,
         dry_run_low/1]).


run() ->
    updo_low:translate_and_run(high_level_script()).

dry_run() ->
    high_level_script().

dry_run_low() ->
    {_, LLinstrs} = updo_low:translate_and_check(high_level_script()),
    LLinstrs.

%%% The following functions are the same as above, except they take a file
%%% containing additional high or low level instructions that are appended to
%%% the automatically generated script.

run(Filename) ->
    updo_low:translate_and_run(high_level_script(Filename)).

dry_run(Filename) ->
    high_level_script(Filename).

dry_run_low(Filename) ->
    {_, LLinstrs} = updo_low:translate_and_check(high_level_script(Filename)),
    LLinstrs.


%%% ========================================================================
%%% Internal
%%% ========================================================================

high_level_script() ->
    updo_high:script(updated_modules()).

high_level_script(Filename) ->
    high_level_script() ++ read_script(Filename).

updated_modules() ->
    [ MF || {Mod, Filename} = MF <- code:all_loaded(),
                                    is_list(Filename),
                                    not code:is_sticky(Mod),
                                    is_updated(Mod, Filename) ].

is_updated(Module, Filename) ->
    LoadedVer = proplists:get_value(vsn, Module:module_info(attributes)),
    case beam_lib:version(Filename) of
        {ok, {_, FileVer}} -> FileVer /= LoadedVer;
        {error, _, _}      -> false
    end.

read_script(Filename) ->
    case file:consult(Filename) of
	{ok, HLinstrs} ->
	    HLinstrs;
	{error, Error} when is_tuple(Error) ->
	    io:format("Failed to parse line ~s~n", [file:format_error(Error)]),
	    exit(parse_error);
	{error, Error} ->
	    io:format("Failed to open file: ~s~n", [file:format_error(Error)]),
	    exit(Error)
    end.
