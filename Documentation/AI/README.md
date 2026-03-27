# Documentation/AI — Focused Context Files for AI Agents

This directory contains task-scoped context files loaded on demand by AI agents.
They are the second layer of a two-layer agent guidance system:

```
AGENTS.md                  ← Layer 1: routing layer (always loaded)
Documentation/AI/*.md      ← Layer 2: focused context (loaded on demand)
```

## Design Principle

`AGENTS.md` is intentionally minimal. It contains only:
- A routing table mapping task types to files in this directory
- Non-discoverable critical information every agent needs immediately
  (pitfalls, AI contribution policy, resource URLs)

Everything else lives here — detailed enough to be useful, scoped enough to
avoid loading irrelevant context into the agent's working window.

## Existing Files
Add file to table in AGENTS.md for routing:

| File | When to load |
|------|-------------|
| `building.md` | Configuring or building ITK with Pixi or CMake |
| `testing.md` | Writing, running, or converting tests |
| `new_task.md` | Conditions for including context from `new_task.md` |
|   ...         | .... |

## Adding a New Context File

1. **Identify the task boundary.** A file should cover one coherent task an
   agent might perform (e.g., "profiling", "Python wrapping", "remote modules").
   If it covers two unrelated topics, split it.

2. **Include only non-discoverable content.** Ask: can an agent learn this
   by reading the source tree, `pixi.toml`, or CMakeLists.txt? If yes, omit
   it. Include only conventions, landmines, and non-obvious workflows that
   aren't expressed in code.

3. **Name the file descriptively** using lowercase with hyphens:
   `python-wrapping.md`, `remote-modules.md`, `performance-profiling.md`.

4. **Structure the file** with a single H1 title and H2 sections. Keep it
   under ~100 lines. If it grows larger, consider splitting.

5. **Register it in `AGENTS.md`** by adding a row to the routing table:

   ```markdown
   | Task description | `Documentation/AI/your-new-file.md` |
   ```

   The task description should match how an agent would naturally describe
   what it is trying to do — not a file name or category label.

6. **Keep it current.** Context files that describe workflows or commands
   drift as the codebase evolves. Update them when the underlying reality
   changes; stale guidance is worse than no guidance.

## What Does Not Belong Here

- Content already documented in `CONTRIBUTING.md`, `GettingStarted.md`,
  or upstream docs (link to those instead)
- Step-by-step tutorials (use the ITK Software Guide or examples.itk.org)
- Ephemeral state: active branches, in-progress bugs, who is working on what
- Code snippets that duplicate what is already in the source tree
