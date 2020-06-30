-module(codeless).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    usage(),
    erlang:halt(1);
main(["init"|_]) ->
    initialize();
main(["koan"]) ->
    get_koan();
main(["koan", Val|_]) ->
    try
        N = list_to_integer(Val),
        get_koan({number, N})
    catch
        error:badarg -> get_koan({koan, Val})
    end;
main(["name", Name|_]) ->
    get_name(Name);
main([Cmd|_]) ->
    io:format("unknown command ~s~n", [Cmd]),
    usage(),
    erlang:halt(1).


%%====================================================================
%% Internal functions
%%====================================================================

usage() ->
    io:format("usage: ~s {init|koan}~n~n", [escript:script_name()]),
    io:format("commands~n"),
    io:format("  init       initialize the database~n"),
    io:format("  koan       get a random koan, or one specified by name or "
        ++ "number~n"),
    io:format("  name       get details about a codeless name~n"),
    io:format("~n").

initialize() ->
    Repo = "https://github.com/aldesantis/the-codeless-code",
    Path = codeless_path(),
    io:format("using path ~s~n", [Path]),
    ok = file:make_dir(Path),
    Cmd = lists:flatten(io_lib:format("clone ~s ~s/repo", [Repo, Path])),
    ok = git_command(Cmd),
    ok = codeless_db:initialize(Path).

get_koan() ->
    Koan = codeless_db:get_koan(codeless_path()),
    print_koan(Koan).

get_koan(Key) ->
    case codeless_db:get_koan(codeless_path(), Key) of
        {koan, Koan} -> print_koan(Koan);
        undefined -> io:format("no koan found~n")
    end.

print_koan(Koan) ->
    io:format("~s~nCase ~p~n",
              [maps:get(title, Koan), maps:get(number, Koan)]),
    case maps:get(names, Koan, []) of
        [] -> io:format("~n");
        Names -> io:format("References: ~s~n~n", [string:join(Names, ", ")])
    end,
    io:format("~s~n", [maps:get(content, Koan)]).

get_name(Name) ->
    case codeless_db:get_name(codeless_path(), Name) of
        {name, N} -> print_name(N);
        undefined -> io:format("no name found~n")
    end.

print_name(Name) ->
    io:format("~s~n~n", [maps:get(name, Name)]),
    case maps:get(content, Name, undefined) of
        undefined -> ok;
        Content -> io:format("~s~n", [Content])
    end,
    case maps:get(references, Name, []) of
        [] -> ok;
        References ->
            io:format("Referenced in:~n"),
            lists:foreach(fun print_reference/1, References)
    end.

print_reference(Reference) ->
    {koan, Koan} = codeless_db:get_koan(codeless_path(), {number, Reference}),
    io:format("~s - ~s~n", [string:right([$#|integer_to_list(Reference)], 5),
                            maps:get(title, Koan)]).

codeless_path() ->
    os:getenv("HOME") ++ "/.codeless".

git_command(Cmd) ->
    {0, _Data} = exec("git " ++ Cmd),
    ok.

exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT',  Port,  _} ->
                    ok
            after 1 ->              % force context switch
                      ok
            end,
            ExitCode =
            receive
                {Port, {exit_status, Code}} ->
                    Code
            end,
            {ExitCode, lists:flatten(Sofar)}
    end.
