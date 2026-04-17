# ITK Enforced Code Style

Style rules enforced by pre-commit hooks and CI. Violations block commits
and PRs. See [compiler-cautions.md](./compiler-cautions.md) section 12 for
KWStyle-specific pitfalls.

## First-Time Setup

```bash
./Utilities/SetupForDevelopment.sh
```

Configures git hooks (pre-commit clang-format, commit-msg KWStyle check) and
remote setup. Required before your first commit.

## C++ Formatting (clang-format)

clang-format 19.1.7 is enforced automatically by the pre-commit hook.

```bash
Utilities/Maintenance/clang-format.bash --modified   # Format modified files only
```

The hook modifies files in place; re-stage and recommit if it changes anything.
Do not use `--no-verify` to bypass — the format check exists to keep CI green.

## KWStyle (commit messages and doxygen)

The `kw-commit-msg.py` hook enforces:
- Subject line ≤78 characters
- Approved commit-subject prefix — see
  [git-commits.md § Prefixes](./git-commits.md#prefixes) for the list

KWStyle also checks that every header has the doxygen `\class` tag. See
[compiler-cautions.md](./compiler-cautions.md) section 12a for the
`enum class` pitfall where KWStyle requires `\class` on enum declarations.

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

## CI/CD

- **Azure Pipelines** — Linux, Windows, macOS (C++ and Python)
- **GitHub Actions** — Pixi builds, Apple Silicon
- **CDash** — https://open.cdash.org/index.php?project=Insight
