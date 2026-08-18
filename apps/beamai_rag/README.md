# Agent RAG

[English](README_EN.md) | 中文

检索增强生成（Retrieval-Augmented Generation）模块：文档分割、向量嵌入、
向量存储与相似度检索、Cross-Encoder 重排序，以及结合上下文的 LLM 生成。

嵌入、重排序、生成三项能力都由 `beamai_llm` 提供，本应用负责把它们串成管道。

## 特性

- 文档分割（固定长度 + 重叠）
- 向量嵌入（openai / dashscope / zhipu / siliconflow / ollama / mock，自动分批与重试）
- 内存向量存储与余弦相似度检索
- 重排序（Cross-Encoder 两阶段检索）
- 与 LLM 集成（基于检索上下文生成回答）
- 检索与重排全流程回调

## 模块概览

- **beamai_rag** - RAG 管道主模块（索引 / 检索 / 生成）
- **beamai_embeddings** - 嵌入向量生成（provider 形态委托 beamai_llm 的 `beamai_embedding`）
- **beamai_vector_store** - 内存向量存储
- **beamai_rag_reranker** - 重排序适配（接 beamai_llm 的 `beamai_rerank`）
- **beamai_rag_splitter** - 文档分割器
- **beamai_rag_utils** - 工具函数（向量运算、类型转换）

## 快速开始

```erlang
%% 1. 创建管道（不带参数即全本地：哈希伪向量 + 内存存储）
Rag0 = beamai_rag:new(#{
    embeddings => #{provider => openai, api_key => ApiKey},
    llm => #{provider => openai, model => <<"gpt-4o">>, api_key => ApiKey}
}),

%% 2. 索引文档：分割 → 嵌入 → 存储
{ok, _DocId, Rag1} = beamai_rag:index_document(
    Rag0, <<"Erlang is a programming language...">>, #{source => <<"wiki">>}),

%% 3. 检索并生成回答
{ok, Answer, Rag2} = beamai_rag:query(Rag1, <<"What is Erlang?">>, #{top_k => 3}).
```

> **管道是不可变值**：`index_document` / `query` 都会返回新的管道，
> 必须接住并往下传，否则索引结果会丢失。

## API 文档

### beamai_rag

```erlang
%% 创建管道（直接返回管道，不是 {ok, _}）
beamai_rag:new() -> Pipeline.
beamai_rag:new(Opts) -> Pipeline.

%% 文本分割（用管道配置 / 用临时配置）
beamai_rag:split_text(Pipeline, Text) -> [Chunk].
beamai_rag:split_text(Pipeline, Text, SplitterOpts) -> [Chunk].

%% 索引：返回新管道
beamai_rag:index_document(Pipeline, Content, Metadata) ->
    {ok, DocId, Pipeline1} | {error, Reason}.
beamai_rag:index_document(Pipeline, Content, Metadata, Opts) ->
    {ok, DocId, Pipeline1} | {error, Reason}.

%% 批量索引：入参为 [{Content, Metadata}]
beamai_rag:index_documents(Pipeline, [{Content, Metadata}], Opts) ->
    {ok, [DocId], Pipeline1}.

%% 索引本地文件（自动补 source / filename 元数据）
beamai_rag:index_file(Pipeline, FilePath, Metadata) ->
    {ok, DocId, Pipeline1} | {error, Reason}.

%% 检索（可选重排序）
beamai_rag:retrieve(Pipeline, Query) -> {ok, [SearchResult]} | {error, Reason}.
beamai_rag:retrieve(Pipeline, Query, Opts) -> {ok, [SearchResult]} | {error, Reason}.

%% 检索 + 生成：同样返回新管道
beamai_rag:query(Pipeline, Question) -> {ok, Answer, Pipeline1} | {error, Reason}.
beamai_rag:query(Pipeline, Question, Opts) -> {ok, Answer, Pipeline1} | {error, Reason}.

%% 管道状态
beamai_rag:get_store(Pipeline) -> VectorStore.
beamai_rag:get_document_count(Pipeline) -> non_neg_integer().
```

`retrieve` 选项：

| 选项 | 默认 | 说明 |
|------|------|------|
| `top_k` | 5 | 最终返回条数 |
| `min_score` | 0.0 | 相似度下限（余弦相似度可为负，需要全量候选时传负值） |
| `reranker` | 管道配置 | 覆盖重排配置；`false` 表示本次不重排 |
| `candidate_top_k` | `top_k * 倍数` | 重排前的粗排候选数 |
| `max_retries` / `retry_delay` / `on_retry` | — | 重排请求的重试策略 |
| `callbacks` / `callback_meta` | `#{}` | 回调与回调元数据 |

`query` 选项：`top_k`、`max_context_length`（默认 2000）、`reranker`、
`candidate_top_k`、`callbacks`、`callback_meta`。

检索结果形态（注意文档在 `document` 字段里，不是平铺的 `content`）：

```erlang
#{
    document := #{id := DocId, content := Content,
                  embedding := Vector, metadata := Metadata},
    score := float(),          %% 启用重排后为重排得分
    rerank_score => float(),   %% 仅重排后存在
    vector_score => float()    %% 仅重排后存在：向量阶段原始相似度
}
```

回调（写在 `callbacks` map，参数末位统一附加 `callback_meta`）：

```erlang
#{
    on_retriever_start => fun(Query, Meta) -> ok end,
    on_retriever_end   => fun(Results, Meta) -> ok end,
    on_retriever_error => fun(Reason, Meta) -> ok end,
    on_rerank_start    => fun(Query, Candidates, Meta) -> ok end,
    on_rerank_end      => fun(Results, Meta) -> ok end,
    on_rerank_error    => fun(Reason, Meta) -> ok end
}
```

### beamai_embeddings

```erlang
%% 构造：多形态入口（undefined / 已构造模型 / beamai_embedding 配置 / 选项 map）
beamai_embeddings:new(ModelOrOpts) -> Model.

%% 构造：委托 beamai_embedding（推荐）
beamai_embeddings:new_provider(Provider) -> Model.
beamai_embeddings:new_provider(Provider, Opts) -> Model.

%% 构造：本模块内置的 OpenAI 直连实现（兼容保留）
beamai_embeddings:new_openai() -> Model.
beamai_embeddings:new_openai(Opts) -> Model.

%% 构造：哈希伪向量，无网络（离线测试）
beamai_embeddings:new_local() -> Model.
beamai_embeddings:new_local(Opts) -> Model.

%% 嵌入
beamai_embeddings:embed_text(Model, Text) -> {ok, Vector} | {error, Reason}.
beamai_embeddings:embed_batch(Model, Texts) -> {ok, [Vector]} | {error, Reason}.

%% 便捷函数（按 OPENAI_API_KEY 是否存在选择默认模型）
beamai_embeddings:embed(Text) -> {ok, Vector} | {error, Reason}.
beamai_embeddings:embed_all(Texts) -> {ok, [Vector]} | {error, Reason}.

%% 向量运算
beamai_embeddings:cosine_similarity(V1, V2) -> float().
beamai_embeddings:euclidean_distance(V1, V2) -> float().
beamai_embeddings:normalize(V) -> Vector.
```

### beamai_vector_store

内存实现：文档列表 + ID 索引，检索为全量余弦相似度扫描。

```erlang
beamai_vector_store:new() -> Store.
beamai_vector_store:new(Opts) -> Store.
beamai_vector_store:clear(Store) -> Store.

%% 文档需包含 content 与 embedding，id 由存储生成
beamai_vector_store:add_document(Store, DocInput) -> {ok, DocId, Store1}.
beamai_vector_store:add_documents(Store, [DocInput]) -> {ok, [DocId], Store1}.
beamai_vector_store:get_document(Store, DocId) -> {ok, Document} | {error, not_found}.
beamai_vector_store:delete_document(Store, DocId) -> {ok, Store1} | {error, not_found}.
beamai_vector_store:count(Store) -> non_neg_integer().

%% 检索：选项为 #{top_k, min_score, filter}
beamai_vector_store:search(Store, QueryVector) -> {ok, [SearchResult]}.
beamai_vector_store:search(Store, QueryVector, Opts) -> {ok, [SearchResult]}.

beamai_vector_store:get_all_documents(Store) -> [Document].
beamai_vector_store:get_documents_by_ids(Store, [DocId]) -> [Document].
```

### beamai_rag_splitter

```erlang
%% 先建分割器，再分割（注意参数顺序：Splitter 在前）
beamai_rag_splitter:new() -> Splitter.
beamai_rag_splitter:new(#{chunk_size => 1000,
                          chunk_overlap => 200,
                          separator => <<"\n">>}) -> Splitter.
beamai_rag_splitter:split(Splitter, Text) -> [Chunk].

beamai_rag_splitter:get_chunk_size(Splitter) -> pos_integer().
beamai_rag_splitter:get_overlap(Splitter) -> non_neg_integer().
beamai_rag_splitter:get_separator(Splitter) -> binary().
```

`Chunk` 形态：`#{content := binary(), chunk_id := binary(), chunk_index := non_neg_integer()}`。

### beamai_rag_utils

```erlang
beamai_rag_utils:ensure_binary(Term) -> binary().
beamai_rag_utils:to_float(Term) -> float().
beamai_rag_utils:dot_product(V1, V2) -> float().
beamai_rag_utils:vector_norm(V) -> float().
beamai_rag_utils:safe_divide(A, B) -> float().
beamai_rag_utils:get_opt(Opts, Key, Default) -> term().
beamai_rag_utils:zip_with_index(List) -> [{non_neg_integer(), term()}].
beamai_rag_utils:take(List, N) -> list().
```

## 使用示例

### 批量索引

```erlang
Docs = [
    {<<"Erlang is a programming language...">>, #{source => <<"wiki">>}},
    {<<"OTP is a set of libraries...">>, #{source => <<"docs">>}}
],
{ok, DocIds, Rag1} = beamai_rag:index_documents(Rag0, Docs, #{}).
```

> `index_documents/3` 会**跳过**索引失败的文档（不中断整批），
> 返回的 `DocIds` 长度可能小于入参长度，需要逐条错误时改用 `index_document/3`。

### 自定义文档分割

```erlang
%% 管道级：建管道时给分割器配置
Rag = beamai_rag:new(#{text_splitter => #{chunk_size => 500, chunk_overlap => 100}}),

%% 调用级：只影响这一次分割
Chunks = beamai_rag:split_text(Rag, LongText, #{chunk_size => 500, chunk_overlap => 100}).
```

### 仅检索（不生成）

```erlang
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP">>, #{top_k => 5}),

lists:foreach(fun(#{document := #{content := Content}, score := Score}) ->
    io:format("Score: ~.3f~nContent: ~s~n~n", [Score, Content])
end, Results).
```

### 重排序（两阶段检索）

向量召回负责「广」，重排序负责「准」：先多召回一批候选，再由 Cross-Encoder
重排模型按 query 相关性打分，截取 top_k 送入上下文。
重排能力来自 beamai_llm 的 `beamai_rerank`（siliconflow / dashscope / jina /
cohere / voyage / mock）。

```erlang
Rag = beamai_rag:new(#{
    embeddings => #{provider => siliconflow, api_key => EmbKey},
    reranker => #{
        provider => siliconflow,
        api_key => RerankKey,
        model => <<"BAAI/bge-reranker-v2-m3">>,
        candidate_multiplier => 4,   %% 粗排候选 = top_k * 4（默认 4）
        on_error => fallback         %% fallback（默认）| fail
    }
}),

%% 召回 20 条 → 重排 → 返回最相关的 5 条
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP 监督树"/utf8>>, #{top_k => 5}).
```

调用级控制：

```erlang
%% 本次不重排
beamai_rag:retrieve(Rag, Query, #{top_k => 5, reranker => false}),

%% 本次自定义候选数（不小于 top_k）
beamai_rag:retrieve(Rag, Query, #{top_k => 5, candidate_top_k => 50}),

%% query/3 同样透传 reranker / candidate_top_k
beamai_rag:query(Rag, Question, #{top_k => 3, candidate_top_k => 30}).
```

失败策略：

- `fallback`（默认）：重排请求失败时记 warning，退回向量检索顺序并截到 top_k。
  返回的仍是真实召回结果，只是未经重排
- `fail`：返回 `{error, {rerank_error, Reason}}`，由调用方决定

## 配置选项

### 嵌入模型

嵌入能力来自 beamai_llm 的 `beamai_embedding`（openai / dashscope / zhipu /
siliconflow / ollama / mock），自带按厂商上限自动分批、瞬态错误重试、
维度参数门控。管道里直接给配置即可：

```erlang
Rag = beamai_rag:new(#{
    embeddings => #{
        provider => siliconflow,
        api_key => <<"sk-...">>,
        model => <<"BAAI/bge-m3">>,
        dimensions => 1024,      %% 支持降维的模型才生效，其余厂商自动剔除
        batch_size => 16,        %% 可选，收紧单批条数（不超过厂商上限）
        max_retries => 3         %% 可选，重试策略
    }
}).
```

也可以先建模型再传入，或继续用内置实现：

```erlang
%% 推荐：走 beamai_embedding
Model = beamai_embeddings:new_provider(dashscope, #{api_key => Key}),

%% 兼容保留：本模块内置的 OpenAI 直连实现（base_url 为完整 URL）
Model2 = beamai_embeddings:new_openai(#{api_key => Key}),

%% 离线测试：哈希伪向量，无网络
Model3 = beamai_embeddings:new_local().
```

**检索语义标注**：provider 形态下，`embed_text/2`（查询）自动标 `text_type =>
<<"query">>`，`embed_batch/2`（待索引文档）自动标 `<<"document">>`——
DashScope 等区分查询与文档的模型据此提升召回，其余厂商忽略该参数。
需要固定取值时构造时传 `text_type => <<"document">>`。

### 向量存储

当前只有内存实现，`beamai_vector_store:new/1` 的选项被忽略；
需要持久化时自行落盘或替换实现（管道的 `vector_store` 可直接传入自建存储值）。

```erlang
Rag = beamai_rag:new(#{vector_store => beamai_vector_store:new()}).
```

### LLM（生成阶段）

```erlang
%% 简写：由 beamai_chat_completion:create/2 组装
Rag = beamai_rag:new(#{llm => #{provider => openai, model => <<"gpt-4o">>,
                                api_key => ApiKey}}),

%% 或直接传已建好的 beamai_chat_completion 配置
LLM = beamai_chat_completion:create(deepseek, #{api_key => Key}),
Rag2 = beamai_rag:new(#{llm => LLM}).
```

未配置可用的 LLM 时，`query/2,3` 返回 `{error, {llm_call_failed, ...}}`，
不会用占位文本冒充回答；只做检索请用 `retrieve/2,3`。

### 分割器

```erlang
#{
    text_splitter => #{
        chunk_size => 1000,     %% 每块字节数
        chunk_overlap => 200,   %% 相邻块重叠字节数
        separator => <<"\n">>   %% 分隔符
    }
}
```

## 依赖

| 依赖 | 必需性 | 用途 |
|------|--------|------|
| `beamai_core` | 必需 | `beamai_http`（内置 OpenAI 嵌入实现）、`beamai_id`、`beamai_utils` |
| `beamai_llm` | 可选（`optional_applications`） | `beamai_embedding`（provider 嵌入）、`beamai_rerank`（重排序）、`beamai_chat_completion`（生成） |

不带 `beamai_llm` 运行时，`new_local` 嵌入与内存检索照常工作；
调用 provider 嵌入或重排会返回 `{error, {embedding_unavailable, _}}` /
`{error, {rerank_unavailable, _}}`，生成阶段返回 `{error, {llm_call_failed, ...}}`。

JSON 编解码走 OTP 27 内置的 `json` 模块，不依赖 jsx。

## 许可证

Apache-2.0
