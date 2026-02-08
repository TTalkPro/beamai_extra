# BeamAI Extra - Task List

## Project Restructuring (2026-02-08)

### Completed: Core Library Extraction
- [x] Removed `apps/beamai_core/` - now depends on external beamai
- [x] Removed `apps/beamai_llm/` - now depends on external beamai
- [x] Removed `apps/beamai_memory/` - now depends on external beamai
- [x] Updated `rebar.config` to depend on `https://github.com/TTalkPro/beamai.git`
- [x] Updated project documentation (README.md, README_EN.md)

### Current Project Focus

The project now focuses on extensions to the core BeamAI library:

**Core Dependencies (from BeamAI):**
- beamai_core - Kernel, Process Framework, Graph Engine, HTTP, Behaviours
- beamai_llm - LLM Chat Completion, Output Parser, Providers
- beamai_memory - Memory Store, Snapshot, Checkpoint

**Project Extensions:**
- beamai_tools - Tool system and Middleware
- beamai_agent - Simple Agent implementation (ReAct pattern)
- beamai_deepagent - Deep Agent (SubAgent architecture)
- beamai_a2a - Agent-to-Agent protocol
- beamai_mcp - Model Context Protocol
- beamai_rag - Retrieval-Augmented Generation

---

## Future Enhancement Tasks

### Phase 1: Documentation Cleanup
- [ ] Update docs/ARCHITECTURE.md for new structure
- [ ] Update docs/API_REFERENCE.md for new structure
- [ ] Update docs/DEPENDENCIES.md for new structure
- [ ] Create migration guide for users

### Phase 2: Examples Verification
- [ ] Test all examples with new dependency structure
- [ ] Update example code if needed
- [ ] Add examples showcasing integration with external beamai

### Phase 3: Feature Enhancements (Project Specific)
- [ ] beamai_tools: Add more middleware options
- [ ] beamai_agent: Enhance callback system
- [ ] beamai_deepagent: Improve parallel execution
- [ ] beamai_a2a: Add more protocol features
- [ ] beamai_mcp: Enhance server/client capabilities
- [ ] beamai_rag: Improve vector store integration

### Phase 4: Testing & Quality
- [ ] Ensure all tests pass with external beamai dependency
- [ ] Add integration tests for beamai_extra apps
- [ ] Performance testing with new structure
- [ ] Dialyzer type checking

---

## Notes

### Working with External BeamAI Dependency

When developing beamai_extra:
1. Core functionality (Kernel, Process, Graph, LLM, Memory) comes from external beamai
2. Focus on agent implementations, protocols, tools, and RAG
3. Coordinate with beamai repository for any core changes needed
4. Keep beamai_extra compatible with latest beamai main branch

### Development Workflow

```bash
# Compile (will fetch beamai dependency)
rebar3 compile

# Start shell
rebar3 shell

# Run tests
rebar3 eunit

# Run tests for specific app
rebar3 eunit --app=beamai_agent
```
