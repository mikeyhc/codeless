-module(codeless_db).
-export([initialize/1, get_koan/1, get_koan/2, get_name/2]).

-define(KOAN_DB, "/koan.db").
-define(KOAN_NUM_DB, "/num_to_koan.db").
-define(NAME_DB, "/name.db").

-record(num_to_koan, {number :: string(),
                      koan :: integer()}).

initialize(Path) ->
    Dir = Path ++ "/repo/the-codeless-code/en-qi",
    {ok, Koans} = dets:open_file(koans, [{file, Path ++ ?KOAN_DB},
                                         {keypos, 2}]),
    {ok, Nums} = dets:open_file(num_to_koan, [{file, Path ++ ?KOAN_NUM_DB},
                                              {keypos, 2}]),
    {ok, Names} = dets:open_file(names, [{file, Path ++ ?NAME_DB},
                                         {keypos, 2}]),
    F = fun(File) ->
                case codeless_file:process_file(Dir ++ "/" ++ File) of
                    {koan, Koan} -> add_koan(Koans, Nums, Names, Koan);
                    {name, Name} -> add_name(Names, Name);
                    {Type, _Data} -> io:format("skipping ~p ~s~n", [Type, File])
                end
        end,
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(F, Files),
    dets:close(Koans),
    dets:close(Nums),
    dets:close(Names).

add_koan(Table, Mappings, Names, Koan) ->
    Title = codeless_file:koan_title(Koan),
    Number = codeless_file:koan_number(Koan),
    io:format("adding koan ~s(~p)~n", [Title, Number]),
    case dets:insert_new(Table, Koan) of
        true -> ok;
        false -> io:format("failed to add ~p~n", [Koan])
    end,
    Mapping = #num_to_koan{number=Number, koan=Title},
    case dets:insert_new(Mappings, Mapping) of
        true -> ok;
        false -> io:format("failed to add mapping ~p~n", [Mapping])
    end,
    F = fun(Ref) ->
                NameRef = codeless_file:build_name_reference(Ref, Title),
                add_name(Names, NameRef)
        end,
    lists:foreach(F, codeless_file:koan_names(Koan)).

add_name(Table, Name0) ->
    NameName = codeless_file:name_name(Name0),
    io:format("adding name ~s~n", [NameName]),
    Name = case dets:lookup(Table, NameName) of
               [OldName] -> codeless_file:merge_names(Name0, OldName);
               L=[_|_] -> throw({invalid_state, L});
               [] -> Name0
           end,
    case dets:insert(Table, Name) of
        ok -> ok;
        {error, Reason} ->
            io:format("failed to add name ~p: ~p~n", [Name, Reason])
    end.

get_koan(Path) ->
    {ok, Ref} = dets:open_file(Path ++ ?KOAN_DB),
    AllKoans = dets:foldl(fun(X, Acc) -> [X|Acc] end, [], Ref),
    dets:close(Ref),
    Random = rand:uniform(length(AllKoans)),
    codeless_file:koan_to_map(lists:nth(Random, AllKoans)).

get_koan(Path, {number, Number}) ->
    {ok, Ref} = dets:open_file(Path ++ ?KOAN_NUM_DB),
    MaybeKoan = dets:lookup(Ref, Number),
    dets:close(Ref),
    case MaybeKoan of
        [#num_to_koan{koan=Koan}] -> get_koan(Path, {koan, Koan});
        [_|_] -> throw({invalid_state, MaybeKoan});
        [] -> undefined
    end;
get_koan(Path, {koan, Title}) ->
    {ok, Ref} = dets:open_file(Path ++ ?KOAN_DB),
    MaybeKoan = dets:lookup(Ref, Title),
    dets:close(Ref),
    case MaybeKoan of
        [Koan] -> {koan, codeless_file:koan_to_map(Koan)};
        [_|_] -> throw({invalid_state, MaybeKoan});
        [] -> undefined
    end.

get_name(Path, Name) ->
    {ok, Ref} = dets:open_file(Path ++ ?NAME_DB),
    MaybeName = dets:lookup(Ref, Name),
    dets:close(Ref),
    case MaybeName of
        [N] -> {name, codeless_file:name_to_map(N)};
        [_|_] -> throw({invalid_state, MaybeName});
        [] -> undefined
    end.
