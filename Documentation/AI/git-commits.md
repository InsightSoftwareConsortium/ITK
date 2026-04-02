# ITK Git Commit Guidelines

## Required Format

Enforced by the `kw-commit-msg.py` pre-commit hook. Commits that violate the
format will be rejected.

```
PREFIX: Brief description (≤78 chars on subject line)

Longer explanation if needed. Describe *what* changed and *why*,
not just that a tool made the change.
```

## Prefixes

| Prefix | Use for |
|--------|---------|
| `ENH:` | New feature or enhancement |
| `BUG:` | Bug fix |
| `COMP:` | Compilation fix (warnings, errors) |
| `DOC:` | Documentation only |
| `STYLE:` | Formatting, naming, no logic change |
| `PERF:` | Performance improvement |
| `WIP:` | Work in progress (do not merge) |

## Hook Behavior

The pre-commit hook runs **clang-format** on staged C++ files and modifies
them in place if formatting changes are needed. When this happens:

1. The hook modifies the file(s)
2. The original commit is aborted
3. You must **re-stage** the modified files and **recommit**

Do not use `--no-verify` to bypass hooks — the format check exists to keep
CI green.

## First-Time Setup

Run once after cloning:

```bash
./Utilities/SetupForDevelopment.sh
```

This installs the commit-msg and pre-commit hooks. Without it, commits will
not be validated locally.

## AI-Assisted Commits

Commit messages for AI-assisted changes must still describe **what** changed
and **why** — not merely that an AI tool generated the change. A
`Co-Authored-By: <AI TOOL>` should not be used in commit messages.
See `Documentation/AI/pull-requests.md` for PR-level disclosure requirements.

### Pre-Commit Checklist for C++ Changes

Before committing any C++ code, an AI agent must complete all three steps:

1. **Review compiler cautions** — Read
   [compiler-cautions.md](./compiler-cautions.md) and verify the changed code
   does not introduce any of the documented pitfalls. Use the Quick-Reference
   Checklist in that file.

2. **Compile the changed code** — Build the affected module(s) locally.
   See [building.md](./building.md) for build instructions. A commit must not
   introduce a compilation error.

3. **Run relevant tests** — Execute the tests that exercise the changed code.
   See [testing.md](./testing.md) for how to run a targeted subset with
   `ctest -R <pattern>`.
