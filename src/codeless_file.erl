-module(codeless_file).

-export([process_file/1, koan_to_map/1, koan_title/1, koan_number/1,
         name_name/1, name_to_map/1, merge_names/2, koan_names/1,
         build_name_reference/2]).

-record(codeless_koan, {title :: string(),
                        number :: integer(),
                        geekiness :: non_neg_integer(),
                        names :: [string()],
                        content :: string()
                        }).

-record(codeless_name, {name :: string(),
                        content :: string() | undefined,
                        references :: [string()]
                       }).

process_file(Path) ->
    io:format("reading file ~s~n", [Path]),
    {ok, Bin} = file:read_file(Path),
    String = binary:bin_to_list(Bin),
    Lines = string:split(String, "\n", all),
    {Metadata0, [_|Content]} = lists:splitwith(fun not_empty/1, Lines),
    F = fun(X) -> case string:split(X, ": ") of
                      [H,T] -> {H, T};
                      [H] -> {string:slice(H, 0, length(H) - 1), []}
                  end
        end,
    Metadata1 = lists:map(F, Metadata0),
    build_object(Metadata1, string:join(Content, "\n")).

not_empty([]) -> false;
not_empty(_) -> true.

build_object(Metadata, Content) ->
    case lists:keymember("Date", 1, Metadata) of
        true -> {koan, build_koan(Metadata, Content)};
        false -> case lists:keymember("Name", 1, Metadata) of
                     true -> {name, build_name(Metadata, Content)};
                     false -> {unknown, {Metadata, Content}}
                 end
    end.

keyfind(Key, N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        {_, V} -> V;
        false -> Default
    end.

build_koan(Metadata, Content0) ->
    {_, Title0} = lists:keyfind("Title", 1, Metadata),
    Title = case lists:keyfind("Subtitle", 1, Metadata) of
                {_, Subtitle} -> Title0 ++ " - " ++ Subtitle;
                false -> Title0
            end,
    Number = keyfind("Number", 1, Metadata, "0"),
    Geekiness = keyfind("Geekiness", 1, Metadata, "0"),
    Names = string:split(keyfind("Names", 1, Metadata, []), ", ", all),
    Content = strip_tags(Content0),
    #codeless_koan{title=Title,
                   number=list_to_integer(Number),
                   geekiness=list_to_integer(Geekiness),
                   names=Names,
                   content=Content}.

build_name(Metadata, Content0) ->
    {_, Name} = lists:keyfind("Name", 1, Metadata),
    Content = strip_tags(Content0),
    #codeless_name{name=Name,
                   content=Content,
                   references=[]}.

strip_tags(C) -> strip_tags(C, false, undefined, []).

strip_tags([], false, undefined, Acc) -> lists:reverse(Acc);
strip_tags("[[#" ++ T0, State, _Ref, Acc) ->
    {Ref, T} = read_ref(T0),
    strip_tags(T, State, Ref, Acc);
strip_tags("[[" ++ T, State, Ref, Acc) ->
    strip_tags(T, State, Ref, Acc);
strip_tags("]]" ++ T, State, Ref, Acc) ->
    if Ref =/= undefined -> strip_tags(T, State, undefined, Ref ++ Acc);
       true -> strip_tags(T, State, Ref, Acc)
    end;
strip_tags("&nbsp;" ++ T, State, Ref, Acc) ->
    strip_tags(T, State, Ref, [$ |Acc]);
strip_tags("&gt;" ++ T, State, Ref, Acc) ->
    strip_tags(T, State, Ref, [$>|Acc]);
strip_tags("&lt;" ++ T, State, Ref, Acc) ->
    strip_tags(T, State, Ref, [$<|Acc]);
strip_tags("&amp;" ++ T, State, Ref, Acc) ->
    strip_tags(T, State, Ref, [$&|Acc]);
strip_tags("<p " ++ T, _State, Ref, Acc) ->
    strip_tags(T, true, Ref, [$\n|Acc]);
strip_tags("<br " ++ T, _State, Ref, Acc) ->
    strip_tags(T, true, Ref, [$\n|Acc]);
strip_tags([$<|T], _State, Ref, Acc) ->
    strip_tags(T, true, Ref, Acc);
strip_tags([$>|T], true, Ref, Acc) ->
    strip_tags(T, false, Ref, Acc);
strip_tags([_|T], true, Ref, Acc) ->
    strip_tags(T, true, Ref, Acc);
strip_tags([H|T], false, Ref, Acc) ->
    strip_tags(T, false, Ref, [H|Acc]).

koan_to_map(#codeless_koan{title=T, number=N, geekiness=G, names=Names,
                           content=C}) ->
    #{title => T,
      number => N,
      geekiness => G,
      names => Names,
      content => C
     }.

name_to_map(#codeless_name{name=N, content=C, references=Ref}) ->
    #{name => N,
      content => C,
      references => Ref
     }.

koan_title(#codeless_koan{title=T}) -> T.

koan_number(#codeless_koan{number=N}) -> N.

koan_names(#codeless_koan{names=N}) -> N.

name_name(#codeless_name{name=N}) -> N.

build_name_reference(Name, Ref) ->
    #codeless_name{name=Name, references=[Ref]}.

read_ref(T) -> read_ref(T, [$#]).

read_ref([$||T], Acc) -> {[$)|Acc] ++ [$(], T};
read_ref(L=[$]|_], Acc) -> {Acc, L};
read_ref([H|T], Acc) -> read_ref(T, [H|Acc]).

merge_names(A, B) ->
    #codeless_name{name=NameA, content=ContentA, references=RefA} = A,
    #codeless_name{name=NameB, content=ContentB, references=RefB} = B,
    NameA = NameB,
    Content = case {ContentA, ContentB} of
                  {undefined, C} -> C;
                  {C, _} -> C
              end,
    Ref = lists:sort(sets:to_list(sets:from_list(RefA ++ RefB))),
    #codeless_name{name=NameA, content=Content, references=Ref}.
