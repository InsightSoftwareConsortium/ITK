# ITK Code Style and Workflow

## First-Time Setup

```bash
./Utilities/SetupForDevelopment.sh
```

Configures git hooks (pre-commit clang-format, commit-msg KWStyle check) and
remote setup. Required before your first commit.

## C++ Formatting

clang-format 19.1.7 is enforced automatically by the pre-commit hook.

```bash
Utilities/Maintenance/clang-format.bash --modified   # Format modified files only
```

The hook modifies files in place; re-stage and recommit if it changes anything.

## Naming Conventions

| Entity | Convention | Example |
|--------|-----------|---------|
| Classes | PascalCase | `MedianImageFilter` |
| Variables | camelCase | `imageSize` |
| Member variables | `m_` prefix | `m_Radius` |
| Template parameters | `T` or `V` prefix | `TInputImage`, `VDimension` |
| Macros | ALL_CAPS | `ITK_UPPERCASE_MACRO` |

## Style Rules

- `constexpr` instead of `#define` for constants
- Smart pointers for all ITK objects: `auto image = ImageType::New();`
- American English spelling throughout
- Doxygen comments use backslash style: `\class`, `\brief`
- No `using namespace` in headers

## Commit Format

Enforced by `kw-commit-msg.py` hook. Subject line ≤78 characters.

```
PREFIX: Brief description

Longer explanation if needed.
```

Prefixes: `ENH:` `BUG:` `COMP:` `DOC:` `STYLE:` `PERF:` `WIP:`

## CI/CD

- **Azure Pipelines** — Linux, Windows, macOS (C++ and Python)
- **GitHub Actions** — Pixi builds, Apple Silicon
- **CDash** — https://open.cdash.org/index.php?project=Insight
