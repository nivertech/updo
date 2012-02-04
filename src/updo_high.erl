-module(updo_high).

-export([script/1]).

%%%
%%% Generate high level upgrade scripts from the instruction set of
%%% appup files.
%%%
%%% See http://www.erlang.org/doc/man/appup.html
%%%     http://www.erlang.org/doc/design_principles/appup_cookbook.html
%%%

%% Return a list of upgrade instructions for the given list of modules.
%%
script(ModFiles) ->
    lists:map(fun instruction/1, dependencies(ModFiles)).


%%% ========================================================================
%%% Internal
%%% ========================================================================

%% Return the upgrade instruction for a module.
%%
instruction({Mod, Deps}) ->
    case is_supervisor(Mod) of
        true ->
            {update, Mod, supervisor};
        false ->
            case is_special(Mod) of
                true ->
                    {update, Mod, {advanced, []}, Deps};
                false ->
                    {load_module, Mod, Deps}
            end
    end.

%% Establish the dependencies between a list of modules.
%%
%% A tuple is returned for each module with its name and a (possibly
%% empty) list of the other modules it makes calls to.
%%
dependencies(ModFiles) ->
    {Mods, Filenames} = lists:unzip(ModFiles),
    {ok, Xref} = xref:start([{xref_mode, modules}]),
    {ok, Calls} =
        try add_files_to_xref(Xref, Filenames),
            xref:q(Xref, "strict ME || AM")
        after
            xref:stop(Xref)
        end,
    [ {Caller, proplists:get_all_values(Caller, Calls)} || Caller <- Mods ].

add_files_to_xref(Xref, [Filename|T]) ->
    {ok, _} = xref:add_module(Xref, Filename, [{warnings, false}]),
    add_files_to_xref(Xref, T);
add_files_to_xref(_, []) ->
    ok.

%% Is this the module for a "special process" (as it is known in OTP),
%% meaning a process running under a supervisor.
%%
is_special(Mod) ->
    Exports = Mod:module_info(exports),
    lists:member({code_change, 3}, Exports)
        orelse lists:member({system_code_change, 4}, Exports).

is_supervisor(Mod) ->
    Attrs = Mod:module_info(attributes),
    lists:member(supervisor, proplists:get_value(behaviour, Attrs, [])
                          ++ proplists:get_value(behavior, Attrs, [])).
