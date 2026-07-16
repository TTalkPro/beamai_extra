%%%-------------------------------------------------------------------
%%% @doc beamai_rag_splitter 单元测试
%%%
%%% 重点回归：chunk_overlap 是**字节**数，而 UTF-8 里一个汉字占 3 字节。
%%% 旧的 extract_overlap/2 直接 binary:part/3 取末尾 N 字节，几乎必然从某个字
%%% 中间切开——那半截字节成为下一块的开头，于是**除第一块外全是非法 UTF-8**，
%%% 一路喂给 embedding 和 LLM 全是乱码。这个模块此前零测试。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_splitter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% UTF-8 安全（回归测试）
%%====================================================================

%% 每一块都必须是合法 UTF-8
chinese_chunks_are_valid_utf8_test() ->
    Chunks = split_chinese(#{chunk_size => 40, chunk_overlap => 10}),
    ?assert(length(Chunks) > 1),
    [?assert(is_valid_utf8(C)) || C <- contents(Chunks)].

%% 多组 size/overlap 组合都不该切坏字符
chinese_valid_across_configs_test_() ->
    [{lists:flatten(io_lib:format("size=~p overlap=~p", [S, O])),
      ?_assert(lists:all(fun is_valid_utf8/1,
                         contents(split_chinese(#{chunk_size => S, chunk_overlap => O}))))}
     || {S, O} <- [{40, 10}, {30, 7}, {60, 20}, {25, 1}, {50, 11}, {90, 31}]].

%% 纯 ASCII 不受影响
ascii_chunks_are_valid_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 20, chunk_overlap => 5, separator => <<"\n">>}),
    Text = <<"first segment here\nsecond segment here\nthird segment here">>,
    Chunks = beamai_rag_splitter:split(S, Text),
    [?assert(is_valid_utf8(C)) || C <- contents(Chunks)].

%% 重叠仍然存在——不能为了合法性把重叠整个丢掉
overlap_is_still_present_test() ->
    [_First, Second | _] = contents(split_chinese(#{chunk_size => 40, chunk_overlap => 10})),
    %% 第二块以上一块尾部的完整汉字开头
    ?assert(byte_size(Second) > 0),
    ?assert(is_valid_utf8(Second)),
    ?assertNotEqual(nomatch, binary:match(Second, <<"第二段落"/utf8>>)).

%%====================================================================
%% 末尾重叠不能变成独立块
%%====================================================================

%% 回归测试：段落耗尽时残留的重叠尾巴是上一块的纯复制，
%% 旧实现把它当成独立块输出，会被单独 embed 入库、检索时命中重复内容。
trailing_overlap_is_not_emitted_as_chunk_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 10, chunk_overlap => 5, separator => <<"\n">>}),
    ?assertEqual([<<"aaaaaaaaaaaa">>],
                 contents(beamai_rag_splitter:split(S, <<"aaaaaaaaaaaa">>))).

%% 任何一块都不该是前一块尾部的纯子串（即纯重复内容）
no_chunk_is_pure_duplicate_of_predecessor_test() ->
    Chunks = contents(split_chinese(#{chunk_size => 40, chunk_overlap => 10})),
    Pairs = lists:zip(lists:droplast(Chunks), tl(Chunks)),
    [?assertNotEqual(nomatch_expected,
                     case binary:match(Prev, Next) of
                         nomatch -> nomatch_ok;
                         _ -> nomatch_expected   %% Next 完全包含于 Prev = 纯重复
                     end)
     || {Prev, Next} <- Pairs].

%%====================================================================
%% chunk_size 必须是硬上限
%%====================================================================

%% 回归测试：旧实现先 append 再判断，超长段落根本没机会被切开。
%% size=10 喂 26 字节无分隔符文本，输出是一个 26 字节的块。
oversized_segment_is_split_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 10, chunk_overlap => 0, separator => <<"\n">>}),
    Chunks = contents(beamai_rag_splitter:split(S, <<"0123456789ABCDEFGHIJKLMNOP">>)),
    ?assert(length(Chunks) > 1),
    [?assert(byte_size(C) =< 10) || C <- Chunks],
    %% 内容不能丢
    ?assertEqual(<<"0123456789ABCDEFGHIJKLMNOP">>, iolist_to_binary(Chunks)).

chunk_size_is_respected_across_configs_test_() ->
    Text = <<"aaaaaaaaaaaaaaa\nbbbbbbbbbbbbbbb\nccccccccccccccc">>,
    [{lists:flatten(io_lib:format("size=~p overlap=~p 时无超限块", [S, O])),
      ?_assert(begin
          Sp = beamai_rag_splitter:new(#{chunk_size => S, chunk_overlap => O,
                                         separator => <<"\n">>}),
          Cs = contents(beamai_rag_splitter:split(Sp, Text)),
          lists:all(fun(C) -> byte_size(C) =< S end, Cs)
      end)}
     || {S, O} <- [{20, 5}, {10, 0}, {16, 4}, {50, 10}]].

%% 超长中文段落硬切后仍须字符完整
oversized_chinese_segment_stays_valid_utf8_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 10, chunk_overlap => 0, separator => <<"\n">>}),
    Long = <<"这是一个没有任何分隔符的很长的中文段落"/utf8>>,
    Chunks = contents(beamai_rag_splitter:split(S, Long)),
    ?assert(length(Chunks) > 1),
    [?assert(byte_size(C) =< 10) || C <- Chunks],
    [?assert(is_valid_utf8(C)) || C <- Chunks],
    ?assertEqual(Long, iolist_to_binary(Chunks)).

%% chunk_size 小于单个字符的字节数：必须推进，不能死循环
chunk_size_smaller_than_one_char_terminates_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 1, chunk_overlap => 0, separator => <<"\n">>}),
    Chunks = contents(beamai_rag_splitter:split(S, <<"中文"/utf8>>)),
    %% 至少取一个完整字符，宁可超过 size 也不能切坏字符或空转
    ?assertEqual(2, length(Chunks)),
    [?assert(is_valid_utf8(C)) || C <- Chunks],
    ?assertEqual(<<"中文"/utf8>>, iolist_to_binary(Chunks)).

%%====================================================================
%% 基本切分行为
%%====================================================================

empty_text_test() ->
    S = beamai_rag_splitter:new(#{}),
    ?assertEqual([], beamai_rag_splitter:split(S, <<>>)).

single_short_segment_is_one_chunk_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 1000, chunk_overlap => 0}),
    ?assertMatch([#{content := <<"hello">>}], beamai_rag_splitter:split(S, <<"hello">>)).

chunk_index_is_sequential_test() ->
    Chunks = split_chinese(#{chunk_size => 40, chunk_overlap => 10}),
    Idx = [maps:get(chunk_index, C) || C <- Chunks],
    ?assertEqual(lists:seq(0, length(Chunks) - 1), Idx).

chunk_ids_are_unique_test() ->
    Chunks = split_chinese(#{chunk_size => 40, chunk_overlap => 10}),
    Ids = [maps:get(chunk_id, C) || C <- Chunks],
    ?assertEqual(length(Ids), length(lists:usort(Ids))).

zero_overlap_test() ->
    Chunks = contents(split_chinese(#{chunk_size => 40, chunk_overlap => 0})),
    [?assert(is_valid_utf8(C)) || C <- Chunks].

%% 分隔符 trim_all：空段落不该产出空块
blank_segments_are_dropped_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 1000, chunk_overlap => 0, separator => <<"\n">>}),
    Chunks = beamai_rag_splitter:split(S, <<"a\n\n\nb">>),
    [?assertNotEqual(<<>>, C) || C <- contents(Chunks)].

%%====================================================================
%% 配置访问
%%====================================================================

config_accessors_test() ->
    S = beamai_rag_splitter:new(#{chunk_size => 123, chunk_overlap => 45, separator => <<"|">>}),
    ?assertEqual(123, beamai_rag_splitter:get_chunk_size(S)),
    ?assertEqual(45, beamai_rag_splitter:get_overlap(S)),
    ?assertEqual(<<"|">>, beamai_rag_splitter:get_separator(S)).

defaults_test() ->
    S = beamai_rag_splitter:new(#{}),
    ?assert(is_integer(beamai_rag_splitter:get_chunk_size(S))),
    ?assert(is_integer(beamai_rag_splitter:get_overlap(S))),
    ?assert(beamai_rag_splitter:get_overlap(S) < beamai_rag_splitter:get_chunk_size(S)).

%%====================================================================
%% 辅助
%%====================================================================

%% 中文测试文本：每字 3 字节，最容易暴露按字节切的问题
chinese_text() ->
    <<"第一段落内容比较长需要被切分开来"/utf8, "\n",
      "第二段落内容也不短同样需要切分"/utf8, "\n",
      "第三段落用来触发第二次切分动作"/utf8>>.

split_chinese(Opts) ->
    S = beamai_rag_splitter:new(maps:merge(#{separator => <<"\n">>}, Opts)),
    beamai_rag_splitter:split(S, chinese_text()).

contents(Chunks) -> [maps:get(content, C) || C <- Chunks].

is_valid_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Result when is_binary(Result) -> true;
        _ -> false
    end.
