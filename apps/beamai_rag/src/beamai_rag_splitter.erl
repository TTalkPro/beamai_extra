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
%% 将按分隔符切分的段落合并为块。
%%
%% chunk_size 是硬上限：当单个段落超过 chunk_size 时，硬切为多个不超过
%% chunk_size 的片段。硬切片段之间保留 overlap 重叠（不插 separator，因为
%% 原文此处本就没有分隔符）。所有切割点对齐 UTF-8 字符边界。
-spec merge_segments([binary()], pos_integer(), non_neg_integer(), binary()) -> [binary()].
merge_segments(Segments, ChunkSize, Overlap, Sep) ->
    merge_loop(Segments, ChunkSize, Overlap, Sep, [], <<>>, true).

%% @private 合并循环
%%
%% 累积段落直到达到块大小，然后开始新块；新块从上一块的重叠部分开始。
%%
%% Seeded 表示 Current 目前**只是**上一块的重叠尾巴、还没接上任何新内容。
%% 段落耗尽时若仍是 Seeded，这段重叠必须丢弃——它是上一块尾部的纯复制，
%% 作为独立块输出会被单独 embed 并入库，检索时命中重复内容。
%%
%% 当段落导致 Current 超过 chunk_size 时：
%% 1. 若 Current 有实质内容（not Seeded）：先 flush Current，取 overlap 作为
%%    新 Current，重新处理该段落；
%% 2. 若 Current 只是 overlap（Seeded）或为空：段落本身超长，走硬切路径。
-spec merge_loop([binary()], pos_integer(), non_neg_integer(), binary(),
                 [binary()], binary(), boolean()) -> [binary()].
merge_loop([], _Size, _Overlap, _Sep, Acc, _Current, true) ->
    lists:reverse(Acc);
merge_loop([], _Size, _Overlap, _Sep, Acc, <<>>, _Seeded) ->
    lists:reverse(Acc);
merge_loop([], _Size, _Overlap, _Sep, Acc, Current, false) ->
    lists:reverse([Current | Acc]);
merge_loop([Seg | Rest], Size, Overlap, Sep, Acc, Current, Seeded) ->
    SepSize = case byte_size(Current) of 0 -> 0; _ -> byte_size(Sep) end,
    PotentialSize = byte_size(Current) + SepSize + byte_size(Seg),
    case PotentialSize > Size of
        false ->
            %% 正常路径：追加段落后检查是否达到块大小
            NewCurrent = append_segment(Current, Seg, Sep),
            case byte_size(NewCurrent) >= Size of
                true ->
                    OverlapText = extract_overlap(NewCurrent, Overlap),
                    merge_loop(Rest, Size, Overlap, Sep, [NewCurrent | Acc], OverlapText, true);
                false ->
                    merge_loop(Rest, Size, Overlap, Sep, Acc, NewCurrent, false)
            end;
        true when not Seeded ->
            %% Current 有实质内容，先 flush 再重试该段落
            OverlapText = extract_overlap(Current, Overlap),
            merge_loop([Seg | Rest], Size, Overlap, Sep, [Current | Acc], OverlapText, true);
        true ->
            %% Current 只是 overlap 或为空，段落本身超长 → 硬切
            FullSeg = case byte_size(Current) of
                0 -> Seg;
                _ -> <<Current/binary, Seg/binary>>
            end,
            %% 防止 overlap >= Size 导致死循环
            EffOverlap = min(Overlap, max(0, Size - 1)),
            Pieces = hard_cut_segment(FullSeg, Size, EffOverlap),
            Complete = lists:droplast(Pieces),
            Last = lists:last(Pieces),
            NewAcc = lists:reverse(Complete, Acc),
            case byte_size(Last) >= Size of
                true ->
                    OverlapText = extract_overlap(Last, Overlap),
                    merge_loop(Rest, Size, Overlap, Sep, [Last | NewAcc], OverlapText, true);
                false ->
                    merge_loop(Rest, Size, Overlap, Sep, NewAcc, Last, false)
            end
    end.

%% @private 硬切超长段落
%%
%% 将 Text 切成多个不超过 Size 字节的片段，相邻片段间保留 Overlap 字节重叠。
%% 不插入 separator。所有切割点对齐 UTF-8 字符边界。
%%
%% 若单个 UTF-8 字符比 Size 大（极端罕见），该字符整块保留（无法再分）。
-spec hard_cut_segment(binary(), pos_integer(), non_neg_integer()) -> [binary()].
hard_cut_segment(Text, Size, Overlap) ->
    hard_cut_loop(Text, Size, Overlap, []).

%% @private 硬切循环
-spec hard_cut_loop(binary(), pos_integer(), non_neg_integer(), [binary()]) -> [binary()].
hard_cut_loop(Text, Size, Overlap, Acc) ->
    TextSize = byte_size(Text),
    case TextSize =< Size of
        true ->
            lists:reverse([Text | Acc]);
        false ->
            CutPos = backward_align(Text, Size),
            ActualCut = case CutPos of
                0 -> first_char_size(Text);
                _ -> CutPos
            end,
            Piece = binary:part(Text, 0, ActualCut),
            OverlapText = extract_overlap(Piece, Overlap),
            Remaining = binary:part(Text, ActualCut, TextSize - ActualCut),
            NextText = case OverlapText of
                <<>> -> Remaining;
                _ -> <<OverlapText/binary, Remaining/binary>>
            end,
            hard_cut_loop(NextText, Size, Overlap, [Piece | Acc])
    end.

%% @private 向后查找最近的 UTF-8 字符边界（<= Pos）
%%
%% 如果 Text[Pos] 是 UTF-8 续接字节（10xxxxxx），说明多字节字符被切开了，
%% 向前退一格直到找到字符起始字节。
-spec backward_align(binary(), non_neg_integer()) -> non_neg_integer().
backward_align(_Text, 0) -> 0;
backward_align(Text, Pos) when Pos >= byte_size(Text) ->
    byte_size(Text);
backward_align(Text, Pos) ->
    case binary:at(Text, Pos) of
        Byte when Byte >= 16#80, Byte =< 16#BF ->
            backward_align(Text, Pos - 1);
        _ ->
            Pos
    end.

%% @private 返回首字符的字节长度
%%
%% UTF-8 编码：0xxxxxxx=1B, 110xxxxx=2B, 1110xxxx=3B, 11110xxx=4B。
%% 用于首字符大于 Size 的极端情况——无法分割，整块保留。
-spec first_char_size(binary()) -> pos_integer().
first_char_size(<<Byte, _/binary>>) ->
    if
        Byte < 16#80 -> 1;
        Byte < 16#E0 -> 2;
        Byte < 16#F0 -> 3;
        true -> 4
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
