# Agent RAG

English | [中文](README.md)

Retrieval-Augmented Generation module: document splitting, vector embedding,
vector storage with similarity search, cross-encoder reranking, and
context-grounded LLM generation.

Embedding, reranking and generation all come from `beamai_llm`; this app wires
them into a pipeline.

## Features

- Document splitting (fixed size with overlap)
- Vector embedding (openai / dashscope / zhipu / siliconflow / ollama / mock, with auto batching and retries)
- In-memory vector store with cosine similarity search
- Reranking (cross-encoder, two-stage retrieval)
- LLM integration (answers grounded in retrieved context)
- Callbacks across retrieval and reranking

## Module Overview

- **beamai_rag** - Main RAG pipeline (index / retrieve / generate)
- **beamai_embeddings** - Embedding generation (provider mode delegates to `beamai_embedding` from beamai_llm)
- **beamai_vector_store** - In-memory vector store
- **beamai_rag_reranker** - Rerank adapter (wraps `beamai_rerank` from beamai_llm)
- **beamai_rag_splitter** - Document splitter
- **beamai_rag_utils** - Utilities (vector math, conversions)

## Quick Start

```erlang
%% 1. Create a pipeline (no options = fully local: hash vectors + memory store)
Rag0 = beamai_rag:new(#{
    embeddings => #{provider => openai, api_key => ApiKey},
    llm => #{provider => openai, model => <<"gpt-4o">>, api_key => ApiKey}
}),

%% 2. Index a document: split -> embed -> store
{ok, _DocId, Rag1} = beamai_rag:index_document(
    Rag0, <<"Erlang is a programming language...">>, #{source => <<"wiki">>}),

%% 3. Retrieve and generate
{ok, Answer, Rag2} = beamai_rag:query(Rag1, <<"What is Erlang?">>, #{top_k => 3}).
```

> **The pipeline is an immutable value**: `index_document` and `query` return a
> new pipeline that must be carried forward, otherwise indexed data is lost.

## API Documentation

### beamai_rag

```erlang
%% Create a pipeline (returns the pipeline directly, not {ok, _})
beamai_rag:new() -> Pipeline.
beamai_rag:new(Opts) -> Pipeline.

%% Text splitting (pipeline config / one-off config)
beamai_rag:split_text(Pipeline, Text) -> [Chunk].
beamai_rag:split_text(Pipeline, Text, SplitterOpts) -> [Chunk].

%% Indexing: returns a new pipeline
beamai_rag:index_document(Pipeline, Content, Metadata) ->
    {ok, DocId, Pipeline1} | {error, Reason}.
beamai_rag:index_document(Pipeline, Content, Metadata, Opts) ->
    {ok, DocId, Pipeline1} | {error, Reason}.

%% Bulk indexing: input is [{Content, Metadata}]
beamai_rag:index_documents(Pipeline, [{Content, Metadata}], Opts) ->
    {ok, [DocId], Pipeline1}.

%% Index a local file (source / filename metadata added automatically)
beamai_rag:index_file(Pipeline, FilePath, Metadata) ->
    {ok, DocId, Pipeline1} | {error, Reason}.

%% Retrieval (with optional reranking)
beamai_rag:retrieve(Pipeline, Query) -> {ok, [SearchResult]} | {error, Reason}.
beamai_rag:retrieve(Pipeline, Query, Opts) -> {ok, [SearchResult]} | {error, Reason}.

%% Retrieval + generation: also returns a new pipeline
beamai_rag:query(Pipeline, Question) -> {ok, Answer, Pipeline1} | {error, Reason}.
beamai_rag:query(Pipeline, Question, Opts) -> {ok, Answer, Pipeline1} | {error, Reason}.

%% Pipeline state
beamai_rag:get_store(Pipeline) -> VectorStore.
beamai_rag:get_document_count(Pipeline) -> non_neg_integer().
```

`retrieve` options:

| Option | Default | Description |
|--------|---------|-------------|
| `top_k` | 5 | Number of results returned |
| `min_score` | 0.0 | Similarity floor (cosine can be negative — pass a negative value to keep every candidate) |
| `reranker` | pipeline config | Override rerank config; `false` skips reranking for this call |
| `candidate_top_k` | `top_k * multiplier` | Candidate count before reranking |
| `max_retries` / `retry_delay` / `on_retry` | — | Retry policy for the rerank request |
| `callbacks` / `callback_meta` | `#{}` | Callbacks and callback metadata |

`query` options: `top_k`, `max_context_length` (default 2000), `reranker`,
`candidate_top_k`, `callbacks`, `callback_meta`.

Search result shape (the document is nested under `document`, not flattened):

```erlang
#{
    document := #{id := DocId, content := Content,
                  embedding := Vector, metadata := Metadata},
    score := float(),          %% rerank score once reranking is enabled
    rerank_score => float(),   %% present only after reranking
    vector_score => float()    %% present only after reranking: original similarity
}
```

Callbacks (in the `callbacks` map; `callback_meta` is always the last argument):

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
%% Construction: polymorphic entry (undefined / built model / beamai_embedding config / options map)
beamai_embeddings:new(ModelOrOpts) -> Model.

%% Construction: backed by beamai_embedding (recommended)
beamai_embeddings:new_provider(Provider) -> Model.
beamai_embeddings:new_provider(Provider, Opts) -> Model.

%% Construction: this module's built-in OpenAI client (kept for compatibility)
beamai_embeddings:new_openai() -> Model.
beamai_embeddings:new_openai(Opts) -> Model.

%% Construction: deterministic hash vectors, no network (offline testing)
beamai_embeddings:new_local() -> Model.
beamai_embeddings:new_local(Opts) -> Model.

%% Embedding
beamai_embeddings:embed_text(Model, Text) -> {ok, Vector} | {error, Reason}.
beamai_embeddings:embed_batch(Model, Texts) -> {ok, [Vector]} | {error, Reason}.

%% Convenience (default model chosen by presence of OPENAI_API_KEY)
beamai_embeddings:embed(Text) -> {ok, Vector} | {error, Reason}.
beamai_embeddings:embed_all(Texts) -> {ok, [Vector]} | {error, Reason}.

%% Vector math
beamai_embeddings:cosine_similarity(V1, V2) -> float().
beamai_embeddings:euclidean_distance(V1, V2) -> float().
beamai_embeddings:normalize(V) -> Vector.
```

### beamai_vector_store

In-memory implementation: a document list plus an ID index; search is a full
cosine-similarity scan.

```erlang
beamai_vector_store:new() -> Store.
beamai_vector_store:new(Opts) -> Store.
beamai_vector_store:clear(Store) -> Store.

%% Input documents need content and embedding; the store generates the id
beamai_vector_store:add_document(Store, DocInput) -> {ok, DocId, Store1}.
beamai_vector_store:add_documents(Store, [DocInput]) -> {ok, [DocId], Store1}.
beamai_vector_store:get_document(Store, DocId) -> {ok, Document} | {error, not_found}.
beamai_vector_store:delete_document(Store, DocId) -> {ok, Store1} | {error, not_found}.
beamai_vector_store:count(Store) -> non_neg_integer().

%% Search options: #{top_k, min_score, filter}
beamai_vector_store:search(Store, QueryVector) -> {ok, [SearchResult]}.
beamai_vector_store:search(Store, QueryVector, Opts) -> {ok, [SearchResult]}.

beamai_vector_store:get_all_documents(Store) -> [Document].
beamai_vector_store:get_documents_by_ids(Store, [DocId]) -> [Document].
```

### beamai_rag_splitter

```erlang
%% Build the splitter first, then split (note the argument order)
beamai_rag_splitter:new() -> Splitter.
beamai_rag_splitter:new(#{chunk_size => 1000,
                          chunk_overlap => 200,
                          separator => <<"\n">>}) -> Splitter.
beamai_rag_splitter:split(Splitter, Text) -> [Chunk].

beamai_rag_splitter:get_chunk_size(Splitter) -> pos_integer().
beamai_rag_splitter:get_overlap(Splitter) -> non_neg_integer().
beamai_rag_splitter:get_separator(Splitter) -> binary().
```

`Chunk` shape: `#{content := binary(), chunk_id := binary(), chunk_index := non_neg_integer()}`.

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

## Usage Examples

### Bulk Indexing

```erlang
Docs = [
    {<<"Erlang is a programming language...">>, #{source => <<"wiki">>}},
    {<<"OTP is a set of libraries...">>, #{source => <<"docs">>}}
],
{ok, DocIds, Rag1} = beamai_rag:index_documents(Rag0, Docs, #{}).
```

> `index_documents/3` **skips** documents that fail to index instead of aborting
> the batch, so `DocIds` may be shorter than the input. Use `index_document/3`
> when you need per-document errors.

### Custom Document Splitting

```erlang
%% Pipeline level: configure the splitter when creating the pipeline
Rag = beamai_rag:new(#{text_splitter => #{chunk_size => 500, chunk_overlap => 100}}),

%% Call level: affects this split only
Chunks = beamai_rag:split_text(Rag, LongText, #{chunk_size => 500, chunk_overlap => 100}).
```

### Retrieval Only (Without Generation)

```erlang
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP">>, #{top_k => 5}),

lists:foreach(fun(#{document := #{content := Content}, score := Score}) ->
    io:format("Score: ~.3f~nContent: ~s~n~n", [Score, Content])
end, Results).
```

### Reranking (Two-Stage Retrieval)

Vector recall covers breadth, reranking covers precision: pull a wider candidate
set first, then let a cross-encoder rescore them against the query and keep the
top_k for the context. Reranking comes from `beamai_rerank` in beamai_llm
(siliconflow / dashscope / jina / cohere / voyage / mock).

```erlang
Rag = beamai_rag:new(#{
    embeddings => #{provider => siliconflow, api_key => EmbKey},
    reranker => #{
        provider => siliconflow,
        api_key => RerankKey,
        model => <<"BAAI/bge-reranker-v2-m3">>,
        candidate_multiplier => 4,   %% candidates = top_k * 4 (default 4)
        on_error => fallback         %% fallback (default) | fail
    }
}),

%% Recall 20 candidates -> rerank -> return the 5 most relevant
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP supervision">>, #{top_k => 5}).
```

Per-call control:

```erlang
%% Skip reranking for this call
beamai_rag:retrieve(Rag, Query, #{top_k => 5, reranker => false}),

%% Custom candidate count for this call (never below top_k)
beamai_rag:retrieve(Rag, Query, #{top_k => 5, candidate_top_k => 50}),

%% query/3 forwards reranker / candidate_top_k as well
beamai_rag:query(Rag, Question, #{top_k => 3, candidate_top_k => 30}).
```

Failure policy:

- `fallback` (default): log a warning and fall back to vector order truncated to
  top_k — still real recalled results, just not reranked
- `fail`: return `{error, {rerank_error, Reason}}` and let the caller decide

## Configuration Options

### Embedding Model

Embeddings come from `beamai_embedding` in beamai_llm (openai / dashscope /
zhipu / siliconflow / ollama / mock), with automatic batching against each
provider's limit, transient-error retries, and dimension gating. Pass the
config straight into the pipeline:

```erlang
Rag = beamai_rag:new(#{
    embeddings => #{
        provider => siliconflow,
        api_key => <<"sk-...">>,
        model => <<"BAAI/bge-m3">>,
        dimensions => 1024,      %% only for models that support it; dropped otherwise
        batch_size => 16,        %% optional, never exceeds the provider limit
        max_retries => 3         %% optional retry policy
    }
}).
```

You can also build the model first, or keep using the built-in implementations:

```erlang
%% Recommended: backed by beamai_embedding
Model = beamai_embeddings:new_provider(dashscope, #{api_key => Key}),

%% Kept for compatibility: this module's direct OpenAI client (base_url is a full URL)
Model2 = beamai_embeddings:new_openai(#{api_key => Key}),

%% Offline testing: deterministic hash vectors, no network
Model3 = beamai_embeddings:new_local().
```

**Retrieval-aware labelling**: in provider mode, `embed_text/2` (the query) is
tagged `text_type => <<"query">>` and `embed_batch/2` (documents to index) is
tagged `<<"document">>`. Models that distinguish the two (DashScope and others)
use it to improve recall; providers that do not simply ignore it. Pass
`text_type => <<"document">>` at construction time to pin a fixed value.

### Vector Store

Only the in-memory implementation exists today and `beamai_vector_store:new/1`
ignores its options. For persistence, snapshot it yourself or supply your own
store value through the pipeline's `vector_store` option.

```erlang
Rag = beamai_rag:new(#{vector_store => beamai_vector_store:new()}).
```

### LLM (Generation Stage)

```erlang
%% Shorthand: assembled via beamai_chat_model:create/2
Rag = beamai_rag:new(#{llm => #{provider => openai, model => <<"gpt-4o">>,
                                api_key => ApiKey}}),

%% Or pass a ready beamai_chat_model config
LLM = beamai_chat_model:create(deepseek, #{api_key => Key}),
Rag2 = beamai_rag:new(#{llm => LLM}).
```

Without a usable LLM, `query/2,3` returns `{error, {llm_call_failed, ...}}` —
it never passes placeholder text off as an answer. For retrieval only, use
`retrieve/2,3`.

### Splitter

```erlang
#{
    text_splitter => #{
        chunk_size => 1000,     %% bytes per chunk
        chunk_overlap => 200,   %% overlap between adjacent chunks
        separator => <<"\n">>   %% separator
    }
}
```

## Dependencies

| Dependency | Required | Used for |
|------------|----------|----------|
| `beamai_core` | Yes | `beamai_http` (built-in OpenAI embedding client), `beamai_id`, `beamai_utils` |
| `beamai_llm` | Optional (`optional_applications`) | `beamai_embedding` (provider embeddings), `beamai_rerank` (reranking), `beamai_chat_model` (generation) |

Without `beamai_llm`, local embeddings and in-memory retrieval keep working;
provider embeddings and reranking return `{error, {embedding_unavailable, _}}` /
`{error, {rerank_unavailable, _}}`, and generation returns
`{error, {llm_call_failed, ...}}`.

JSON encoding/decoding uses the `json` module built into OTP 27 — no jsx.

## License

Apache-2.0
