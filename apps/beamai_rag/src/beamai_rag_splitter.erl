%%%-------------------------------------------------------------------
%%% @doc RAG 文本分割模块
%%%
%%% 负责将长文本分割为可嵌入的片段（chunk）：
%%% - 按分隔符切分：默认使用换行符
%%% - 尺寸控制：确保每个片段不超过指定大小
%%% - 重叠处理：相邻片段保持部分重叠以保持上下文
%%%
%%% 分割算法：
%%% 1. 按分隔符（如换行）将文本切分为段落
%%% 2. 将段落合并为不超过 chunk_size 的块
%%% 3. 块之间保留 chunk_overlap 字节的重叠
%%%
%%% 设计原则：
%%% - 纯函数：无副作用
%%% - 保持语义：尽量不在句子中间切分
%%% - 可配置：支持自定义分割参数
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_splitter).

%% 导入工具函数
-import(beamai_rag_utils, [zip_with_index/1]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 分割器创建
-export([
    new/0,
    new/1
]).

%% 分割操作
-export([
    split/2
]).

%% 配置访问
-export([
    get_chunk_size/1,
    get_overlap/1,
    get_separator/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type splitter() :: #{
    chunk_size := pos_integer(),
    chunk_overlap := non_neg_integer(),
    separator := binary()
}.

-type chunk() :: #{
    content := binary(),
    chunk_id := binary(),
    chunk_index := non_neg_integer()
}.

-export_type([splitter/0, chunk/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(DEFAULT_CHUNK_SIZE, 1000).
-define(DEFAULT_CHUNK_OVERLAP, 200).
-define(DEFAULT_SEPARATOR, <<"\n">>).

%%====================================================================
%% 分割器创建 API
%%====================================================================

%% @doc 创建默认分割器
-spec new() -> splitter().
new() ->
    new(#{}).

%% @doc 创建自定义分割器
%%
%% 选项：
%% - chunk_size: 每个块的最大字节数（默认 1000）
%% - chunk_overlap: 相邻块的重叠字节数（默认 200）
%% - separator: 切分文本的分隔符（默认 "\n"）
-spec new(map()) -> splitter().
new(Opts) ->
    #{
        chunk_size => maps:get(chunk_size, Opts, ?DEFAULT_CHUNK_SIZE),
        chunk_overlap => maps:get(chunk_overlap, Opts, ?DEFAULT_CHUNK_OVERLAP),
        separator => maps:get(separator, Opts, ?DEFAULT_SEPARATOR)
    }.

%%====================================================================
%% 分割操作 API
%%====================================================================

%% @doc 分割文本为块列表
%%
%% 返回包含内容、ID 和索引的块记录列表。
-spec split(splitter(), binary()) -> [chunk()].
split(Splitter, Text) ->
    #{chunk_size := Size, chunk_overlap := Overlap, separator := Sep} = Splitter,
    Segments = binary:split(Text, Sep, [global, trim_all]),
    ChunkBins = merge_segments(Segments, Size, Overlap, Sep),
    to_chunk_records(ChunkBins).

%%====================================================================
%% 配置访问 API
%%====================================================================

%% @doc 获取块大小配置
-spec get_chunk_size(splitter()) -> pos_integer().
get_chunk_size(#{chunk_size := Size}) -> Size.

%% @doc 获取重叠配置
-spec get_overlap(splitter()) -> non_neg_integer().
get_overlap(#{chunk_overlap := Overlap}) -> Overlap.

%% @doc 获取分隔符配置
-spec get_separator(splitter()) -> binary().
get_separator(#{separator := Sep}) -> Sep.

%%====================================================================
%% 私有函数 - 段落合并
%%====================================================================

%% @private 合并段落为块
%%
%% 将按分隔符切分的段落合并为不超过 ChunkSize 的块。
%% 超长段落（本身就 > ChunkSize）先按字符边界硬切，否则 chunk_size 形同虚设。
-spec merge_segments([binary()], pos_integer(), non_neg_integer(), binary()) -> [binary()].
merge_segments(Segments, ChunkSize, Overlap, Sep) ->
    Splittable = lists:flatmap(fun(S) -> split_oversized(S, ChunkSize) end, Segments),
    merge_loop(Splittable, ChunkSize, Overlap, Sep, [], <<>>, true).

%% @private 把超长段落硬切成不超过 Size 的片段
%%
%% chunk_size 文档写的是「每个块的**最大**字节数」，但旧实现是先 append 再判断
%% `byte_size(NewCurrent) >= Size'——段落本身超长时根本没机会被切开。实测
%% size=10 喂 26 字节无分隔符文本，输出就是一个 26 字节的块。一个没有换行的
%% 文件于是变成单个巨块，直接把 embedding 的 token 上限撑爆。
-spec split_oversized(binary(), pos_integer()) -> [binary()].
split_oversized(Seg, Size) when byte_size(Seg) =< Size ->
    [Seg];
split_oversized(Seg, Size) ->
    Head = take_prefix(Seg, Size),
    HeadSize = byte_size(Head),
    <<_:HeadSize/binary, Rest/binary>> = Seg,
    [Head | split_oversized(Rest, Size)].

%% @private 取至多 Size 字节、且不切断字符的前缀
%%
%% Size 小于一个字符的字节数时必须至少取一个完整字符，否则前缀为空、
%% split_oversized/2 无法推进 → 死循环。
-spec take_prefix(binary(), pos_integer()) -> binary().
take_prefix(Bin, Size) when byte_size(Bin) =< Size ->
    Bin;
take_prefix(Bin, Size) ->
    case trim_to_char_boundary(binary:part(Bin, 0, Size)) of
        <<>> -> first_char(Bin);
        Head -> Head
    end.

%% @private 从尾部回退到最后一个完整字符
-spec trim_to_char_boundary(binary()) -> binary().
trim_to_char_boundary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Valid when is_binary(Valid) -> Valid;
        {incomplete, Valid, _Incomplete} -> Valid;
        %% 非 UTF-8 数据：原样返回，按字节处理总好过丢数据
        {error, _Valid, _Rest} -> Bin
    end.

%% @private 取开头的一个完整字符（保证推进）
-spec first_char(binary()) -> binary().
first_char(<<Byte, _/binary>> = Bin) ->
    Len = utf8_char_len(Byte),
    case byte_size(Bin) >= Len of
        true -> binary:part(Bin, 0, Len);
        false -> Bin
    end.

%% @private 由 UTF-8 首字节判断该字符的字节数
utf8_char_len(B) when B < 16#80 -> 1;
utf8_char_len(B) when B >= 16#C0, B =< 16#DF -> 2;
utf8_char_len(B) when B >= 16#E0, B =< 16#EF -> 3;
utf8_char_len(B) when B >= 16#F0, B =< 16#F7 -> 4;
utf8_char_len(_) -> 1.

%% @private 合并循环
%%
%% 累积段落直到达到块大小，然后开始新块；新块从上一块的重叠部分开始。
%%
%% Seeded 表示 Current 目前**只是**上一块的重叠尾巴、还没接上任何新内容。
%% 段落耗尽时若仍是 Seeded，这段重叠必须丢弃——它是上一块尾部的纯复制，
%% 作为独立块输出会被单独 embed 并入库，检索时命中重复内容。实测
%% size=10 overlap=5 喂 <<"aaaaaaaaaaaa">>，旧实现输出
%% [<<"aaaaaaaaaaaa">>, <<"aaaaa">>]，后者纯属多余。
-spec merge_loop([binary()], pos_integer(), non_neg_integer(), binary(),
                 [binary()], binary(), boolean()) -> [binary()].
merge_loop([], _Size, _Overlap, _Sep, Acc, _Current, true) ->
    %% 只剩重叠尾巴，没有新内容
    lists:reverse(Acc);
merge_loop([], _Size, _Overlap, _Sep, Acc, <<>>, _Seeded) ->
    lists:reverse(Acc);
merge_loop([], _Size, _Overlap, _Sep, Acc, Current, false) ->
    lists:reverse([Current | Acc]);
merge_loop([Seg | Rest], Size, Overlap, Sep, Acc, Current, _Seeded) ->
    NewCurrent = append_segment(Current, Seg, Sep),
    case byte_size(NewCurrent) >= Size of
        true ->
            OverlapText = extract_overlap(NewCurrent, Overlap),
            merge_loop(Rest, Size, Overlap, Sep, [NewCurrent | Acc], OverlapText, true);
        false ->
            merge_loop(Rest, Size, Overlap, Sep, Acc, NewCurrent, false)
    end.

%% @private 追加段落到当前块
-spec append_segment(binary(), binary(), binary()) -> binary().
append_segment(<<>>, Segment, _Sep) -> Segment;
append_segment(Current, Segment, Sep) -> <<Current/binary, Sep/binary, Segment/binary>>.

%% @private 提取重叠文本
%%
%% 从块末尾提取至多 Overlap 字节，作为下一块的起始。
%%
%% 必须对齐到字符边界：Overlap 是**字节**数，而 UTF-8 里一个汉字占 3 字节，
%% 直接 binary:part/3 几乎必然从某个字的中间切开。切出来的半截字节序列会成为
%% 下一块的开头——那块就是非法 UTF-8，一路喂给 embedding 模型和 LLM 全是乱码。
%% 实测（chunk_size=40, overlap=10 的中文文本）4 块里有 3 块非法。
%%
%% 这里向后跳过开头的续接字节（10xxxxxx），即"至多 Overlap 字节，且不切断字符"。
%% 宁可少给几个字节的重叠，也不能给出坏文本。
-spec extract_overlap(binary(), non_neg_integer()) -> binary().
extract_overlap(Text, Overlap) ->
    Size = byte_size(Text),
    case Size > Overlap of
        true -> align_to_char_boundary(binary:part(Text, Size - Overlap, Overlap));
        false -> Text
    end.

%% @private 跳过开头的 UTF-8 续接字节（2#10xxxxxx，即 16#80..16#BF），
%% 使二进制从一个完整字符起始。非 UTF-8 数据最坏情况是原样返回。
-spec align_to_char_boundary(binary()) -> binary().
align_to_char_boundary(<<Byte, Rest/binary>>) when Byte >= 16#80, Byte =< 16#BF ->
    align_to_char_boundary(Rest);
align_to_char_boundary(Bin) ->
    Bin.

%%====================================================================
%% 私有函数 - 块记录构建
%%====================================================================

%% @private 将二进制块列表转换为块记录列表
-spec to_chunk_records([binary()]) -> [chunk()].
to_chunk_records(ChunkBins) ->
    [#{content => C, chunk_id => beamai_id:gen_id(<<"chunk">>), chunk_index => I}
     || {I, C} <- zip_with_index(ChunkBins)].
