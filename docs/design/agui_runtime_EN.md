# AG-UI Runtime (`beamai_agui`): Design Decisions

## Background

CopilotKit's `@copilotkit/runtime` is the server half of the AG-UI protocol: it accepts
a `RunAgentInput` from the frontend and translates one agent execution into an SSE event
stream the client can render incrementally. The idea applies equally well to BeamAI —
the only question was **which repository it belongs in**.

## Decision 1: beamai_extra, not beamai

| | beamai (upstream core) | beamai_extra |
|---|---|---|
| Dependencies | `gun` / `uuid` / `esqlite` / `poolboy`, **no cowboy** | already has `cowboy 2.12` |
| Charter | ChatClient base + LLM client + SimpleAgent | Tools / RAG / protocol implementations |
| Existing protocol servers | none | `beamai_a2a`, `beamai_mcp` |

An AG-UI runtime is fundamentally an HTTP + SSE **server**. Putting it upstream would
drag cowboy in and force changes to the relx release, the dialyzer plt, and the library's
stated charter.

Meanwhile `beamai_a2a` is the same shape of thing, just a different protocol:

| CopilotKit runtime | existing counterpart in beamai_a2a |
|---|---|
| `createCopilotRuntimeHandler` (fetch primitive) | `beamai_a2a_http_handler` (framework-agnostic) |
| Express/Hono adapter | `beamai_a2a_cowboy_handler` |
| `AgentRunner` / thread persistence | `beamai_a2a_task` + `beamai_a2a_task_sup` |
| `hooks.onRequest` / middleware | `beamai_a2a_middleware` |
| SSE response stream | `beamai_sse` (in beamai_core) |

Note the last row: **protocol primitives live in core, protocol servers live in extra**.
That seam already exists, and the AG-UI runtime lands on its upper side.

It is a **sibling of A2A, not a duplicate**: A2A is agent-to-agent, AG-UI is
agent-to-frontend (token-by-token rendering, tool-call visualization, HITL).

## Layering

```
Frontend (CopilotKit / any AG-UI client)
        │  POST /agui/run   ──►  SSE event stream
        ▼
beamai_agui_cowboy_handler      adapter: writes frames, owns the connection
        │
beamai_agui_http_handler        framework-agnostic: parse, validate, resolve agent
        │
beamai_agui_runner              orchestrates one run (fresh / resumed)
        │
beamai_agui_emitter             translation layer: agent callbacks → AG-UI events
        │
beamai_agent                    ReAct loop (upstream)
```

Supporting modules: `beamai_agui_registry` (agent factory table), `beamai_agui_types`
(`RunAgentInput` parsing), `beamai_agui_event` (event construction + SSE encoding),
`beamai_agui_thread` (pending pauses).

**SSE frame encoding belongs to the adapter**, not to the framework-agnostic layer —
streaming writes differ a lot between frameworks (cowboy's `stream_body`, other
frameworks' chunked writers). Leaving it to the adapter is what makes `http_handler`
genuinely framework-agnostic.

## Event mapping

| AG-UI event | driven by |
|---|---|
| `RUN_STARTED` / `RUN_FINISHED` / `RUN_ERROR` | emitted by the runner around the agent |
| `STEP_STARTED` / `STEP_FINISHED` | `on_llm_call` / `on_llm_result` (per iteration) |
| `TEXT_MESSAGE_START` | emitted **lazily**, when the first token arrives |
| `TEXT_MESSAGE_CONTENT` | `on_token` (`Meta.message_id` decides ownership) |
| `TEXT_MESSAGE_END` | `on_message_end` |
| `TOOL_CALL_START` / `ARGS` / `END` | the `tool_calls` on the message from `on_message_end` |
| `TOOL_CALL_RESULT` | `on_tool_result` (paired via `Info.tool_call_id`) |
| `STATE_SNAPSHOT` | once per run, echoing the `state` the request carried |
| `STATE_DELTA` | `on_state_change`, diffed against the previous state |

## Decision 2: `TEXT_MESSAGE_START` is emitted lazily

`on_message_start` fires **before** the LLM call (it must — otherwise a streamed token
would arrive with no `messageId` to belong to), and at that point we do not yet know
whether this round will produce text or only `tool_calls`.

So the emitter merely records the id and **waits for the first real token before emitting
START** — a tool-only round then never produces an empty START/END pair.

The non-streaming path (`run/2`, no tokens) is handled by `on_message_end`: seeing a
message that never opened a text stream yet carries content, it emits START + CONTENT +
END in one go. This fallback is load-bearing, not defensive — it is what kept the event
stream valid while resume was still non-streaming.

## Decision 3: the run gets its own process; the connection process only writes frames

Cowboy's `stream_body` may only be called from the request process, while running an
agent blocks synchronously. A single process doing both can only write after the agent
finishes — which is not streaming at all.

Hence: the run process runs the agent and sends events to the request process, which
does nothing but write frames. When the client disconnects, cowboy kills the request
process and the run dies with it — no orphaned LLM call burning money with nobody watching.

## Decision 4: take only the last user message

An AG-UI client uploads the **entire** `messages` array on every request, whereas
`beamai_agent` accepts exactly one new message per call and lets the memory filter
maintain cross-turn history keyed by `conversation_id`.

Replaying the whole array would count history twice. So we take only the last user
message and leave history to memory.

## Decision 5: run persistence adds no storage layer

Upstream `beamai_agent` already has a complete cross-process HITL mechanism: an agent
configured with a `pause_store` persists an **interrupt-state snapshot** keyed by
`conversation_id` when it pauses (`beamai_agent_pause`), and `beamai_agent:resume/3`
**transparently falls back** to that store when the in-process agent has no interrupt
state.

So all that was needed was aligning three ids:

```
AG-UI threadId  ==  beamai conversation_id  ==  pause_store key
```

"Is this request a resume?" then reduces to "does this thread have a pending snapshot in
the store?" — no second run table required (the originally planned `run_sup` plus run
records were never written).

### One full HITL round trip

1. The agent stops to wait for a human → upstream persists the snapshot.
2. The runtime emits `CUSTOM(interrupt)` (carrying `toolCallId` / `toolCallName` / `args`)
   plus `RUN_FINISHED`, and this HTTP request ends.
3. What the frontend sees is a `TOOL_CALL_START/ARGS/END` with **no result** — that is
   the "waiting for you" state.
4. The next request arrives, the runtime finds the pending pause and switches to resume.

### Three sources for the answer

| Priority | Source | Semantics |
|---|---|---|
| 1 | `forwardedProps.resume` = `#{"decision", "payload"}` | full semantics: `approved` / `rejected` / `reply`, may override `args` |
| 2 | a `role=tool` message whose `toolCallId` matches | AG-UI native: a frontend tool finished and returns its result |
| 3 | the last `user` message | chat style: the agent asked, the human just replies |

When converting the payload's binary keys to the atom keys upstream expects, only
**`message` and `args`** are accepted: the request body is external input, and running
`binary_to_atom` on it would hand an attacker a way to blow up the atom table.

### Two sources for `TOOL_CALL_RESULT` (mutually exclusive)

| Decision | Where the result comes from |
|---|---|
| `reply` / rejection / free text | given by the human; the runtime fills it in via `emit_resume_result` |
| `approved` | the server actually executes the tool → `on_tool_result` → emitter |

A single call never produces two results.

### The factory must be replayable

The snapshot is **pure data** (interrupt state, the interrupted tool_call, completed
results, iteration counters). It does **not** store the ChatClient / tools / callbacks
(which contain funs and could not be serialized anyway) — those are rebuilt by the agent
factory on the next request.

Therefore two requests on the same thread must produce an identically configured agent
(same tool set, same system prompt), or the restored interrupt state will not line up
with the new agent's tool table.

### Where pauses are stored

Three options, in priority order: a host-supplied handle (`pause_store`), the built-in DETS
store (`pause_store_file`), or the built-in ETS store (the default). `beamai_agui_sup` starts
the latter two; for the first it starts nothing.

ETS is the default rather than DETS so that development needs no setup and no file appears
on anyone's disk unasked. Production sets `pause_store_file` to survive node restarts — but
**that alone is not enough**: chat memory must become durable too
(`beamai_chat_memory_dets`). The pause store only records *where the run stopped*, not the
conversation history; with only one of the two, a restored session is incomplete.

Durability introduces a problem ETS never had: **abandoned pauses never go away**. The ETS
table is reclaimed when its owner dies; a DETS file keeps them forever. Hence the DETS
backend also offers `conversations/1` and `prune/2` (drop pauses older than a cutoff by
`paused_at`) — outside the behaviour, purely operational.

### Silent degradation when the store is unavailable

Upstream calls `pause_clear` on **every** terminal state. Handing the agent a handle
pointing at a dead process would make even ordinary runs crash on the way out. So
`runtime_config` checks `available_store/0` first and simply omits the `pause_store` key
when it cannot get one — degrading to "this node has no HITL persistence" while
everything else keeps working.

## Five upstream changes made in beamai for this

The runtime is a thin shell; the real capability lives in the agent layer. The following
five items are the "AG-UI needs it, upstream did not have it" gaps, in the order they
were landed:

### A. Expose `tool_call_id` on the tool callbacks

`on_tool_call` / `on_tool_result` used to receive only `(Name, Args)` / `(Name, Result)`.
With `parallel_tools` enabled by default, results fire in nondeterministic order, so
**the tool name alone cannot pair them** (the same tool may appear several times in one
batch). The id was already in the CallRecord — it was simply being dropped.

The fix dispatches on the registered function's **arity**: `Fun/2` is the old signature,
`Fun/3` additionally receives `Info` (`Meta` plus `tool_call_id`; `on_tool_result` also
carries `args` / `error`). Arity dispatch was chosen over changing the signature because
a dozen call sites in upstream tests, downstream examples and docs use the old form.

### B. Expose raw provider stream events

`beamai_chat_client`'s streaming terminal hardcoded the raw-event callback to
`fun(_Event) -> ok end`, and `extract_token_from_event/1` recognizes only four text
shapes. As a result the `arguments` deltas of `tool_calls`, thinking/reasoning deltas and
per-chunk usage were **discarded at that layer** and unreachable to anything downstream.

The fix adds a chat opt `on_raw_event` (consumed by ChatClient itself, stripped before
reaching the provider) and an agent callback `on_llm_event`. It **deliberately does not
normalize**: normalizing here would decide on every host's behalf which fields are worth
keeping, and the users of this channel want precisely the untrimmed original.

### C. Message boundary callbacks

One turn produces more than one assistant message, and `on_token` is a flat token stream
that cannot delimit them.

The fix adds `on_message_start(MessageId, Meta)` and `on_message_end(Message, Meta)`.
**The id is allocated before the LLM call**, so by the time a streamed token arrives it is
already in `on_token`'s `Meta.message_id`. The two are **always paired**: when the LLM
fails or nothing is recorded, `Message` is `undefined`, so hosts need no fallback to close
out error paths.

`record_assistant` is the single authoritative point for "this message ends here", and the
synthesized return-direct turn goes through it too, so boundaries hold for that path as well.

### D. `stream_resume`

`resume/2,3` does not accept a `stream_token_handler`, so the resumed round used the
non-streaming `invoke_chat` and produced no tokens at all — yet the answer after the
human replies is exactly the text the user is waiting for.

The fix adds `resume/4` (the low-level entry point, `Opts` matching `run/3`) and
`stream_resume/2,3` (what `stream` is to `run`). It also repaired a metadata problem:
the tool loop used to put only `message_id` into `Info`, while `stream/3` built its base
`Meta` **before** entering the loop, at which point this run's `run_id` did not exist yet.
Passing the loop's full `Meta` makes all four callbacks agree.

### F. State becomes observable and seedable

The state slots (`beamai_context`'s state) previously had **no outlet**: tools wrote via
`writes` and the barrier folded them, but no callback exposed the result, so a host could
not see changes. Nor was there anywhere to seed an initial value — a turn's context is
built fresh each round, so tools always started from an empty state.

Two changes: the agent config gains `initial_state` (seeded into each turn's context by
`initial_turn_context/1`), and the callbacks gain `on_state_change(State, Meta)`, fired at
the barrier once writes are folded **and the state actually changed**, carrying the whole
folded state. Only firing on change matters — most tools never write state, and firing per
batch would just spam the host. Computing an incremental diff is left to the host: the
framework does not decide the shape of that diff for it.

### E. Tools executed during resume fire `on_tool_result`

`resume` executes tools in two places **outside** the loop (the approved call, and the
retry of environment failures), and both omitted `on_result`, leaving the host with a tool
call that starts and never finishes.

The fix adds `beamai_agent_tool_loop:agent_result_cb/1` and wires it into both sites. It
**deliberately does not** re-run the `on_tool_call` policy gate: a call that a human just
approved would simply be blocked again, forever.

## Decision 8: establish a baseline before sending patches

AG-UI's `STATE_DELTA.delta` is a **JSON Patch array** the frontend applies in place to its
local state. A patch is computed against one specific state — without a shared baseline it
cannot line up, so every run opens with a `STATE_SNAPSHOT` carrying whatever `state` the
request brought.

The differ only produces `add` / `remove` / `replace`: RFC 6902 also has `move` / `copy`
(which require recognizing "the same value moved", meaningless for state slots and easy to
get wrong) and `test` (an assertion). It recurses into maps only; arrays and scalars are
replaced wholesale — element-level array diffing needs an LCS pass to avoid emitting piles
of meaningless shift patches, and the payoff does not justify it. Paths are escaped per
RFC 6901 (`~` → `~0`, `/` → `~1`): slot keys come from the user and may well contain a
slash, and skipping the escape would compute a path pointing somewhere else.

**Resume does not take the frontend's state**: the pause snapshot carries the state the
server accumulated, and `resume` restores from there. For a turn stopped halfway, the
server holds the authoritative state.

By default state is still read only by **tools** (`beamai_context:state_get/3`); the model
does not see it. When the model must reason about current state, the endpoint opts in with
`inject_state => true`: the runtime renders the whole state into system text via
`beamai_agui_types:state_prompt/1` and appends it after context. This is not the default —
it puts the whole UI state into every LLM request, so the token cost and disclosure are
the host's explicit decision. Resume injects `saved_state` from the pause snapshot, not the
`state` resent with the resume request, preserving the rule above that the server is
authoritative.

## Known gaps

### Runtime side

1. **Auth and rate limiting.** `beamai_a2a_auth` / `beamai_a2a_rate_limit` are ready-made
   templates.

### Engineering side

- **Never validated against a real CopilotKit frontend.** Event shapes were written from
  the protocol docs and have not yet been consumed by a real client (a real LLM has been,
  see below).

## Live validation

`examples/src/example_agui_live.erl` wires the whole chain: start cowboy for real, POST with
gun, let a real model decide whether to call tools, and read SSE frames as they arrive —
finally replaying the `STATE_DELTA` patches onto the client-side baseline for comparison.
Both MiniMax `m2.7` and ZHIPU `glm-5.3-flash` pass.

What it demonstrated:

- `TOOL_CALL_ARGS` really does stream character by character (`{"` → `text` → `":"` → `买`
  → `牛奶` → `"` → `}`), so the incremental translation holds against a real provider, not
  just against synthetic chunks;
- `REASONING_*` streams throughout on glm-5.3-flash;
- two concurrent tool calls delivered their `TOOL_CALL_RESULT`s **in the opposite order
  from the calls** — field evidence for why pairing by `tool_call_id` is not optional
  (the reason for change A);
- replaying the patches onto the baseline reconstructs exactly the server's state.

## Decision 6: frontend tools reuse the HITL pipeline

`RunAgentInput.tools` are tools defined and executed by the frontend: the model must be
able to see them (otherwise it will never call them), but the server has no implementation
to run. `interrupt_tools` is exactly that semantic — pause on a hit and hand the ball
outward. Frontend tools therefore travel the **same** pause/resume path as human approval,
with no new machinery.

The runtime attaches them rather than the factory: tools arrive **per request**, whereas
the factory is supposed to be replayable constant configuration (two requests on a thread
must produce an identical agent). They are appended, not substituted, and on a name clash
the server-side tool wins — it has a real implementation, the frontend one is only a
declaration.

Upstream's `interrupt_tool_to_spec` / `is_interrupt_tool` accept both atom and binary keys,
so AG-UI definitions can be attached verbatim with no conversion.

## Decision 7: incremental events and the one-shot fallback are mutually exclusive

`beamai_agui_raw` translates raw chunks into `TOOL_CALL_ARGS` / `REASONING_*` per provider
shape (two branches, OpenAI-style and Anthropic-style; anything unrecognized is dropped
silently).

That incremental path **competes with `on_message_end` for the same `TOOL_CALL_START`** —
the former opens the call when the first args fragment arrives, while the latter holds an
assistant message that also carries the complete `tool_calls`. So the emitter tracks which
ids have already been streamed and emits only a `TOOL_CALL_END` for them; ids that were
not streamed still take the one-shot full-JSON fallback. The two paths are mutually
exclusive: a single call only ever gets one START.

A reasoning run is closed at either of two points — when the answer starts (`on_token`) or
when the message lands (`on_message_end`) — always emitting `REASONING_MESSAGE_END` before
`REASONING_END`.
