# Migration sentinels

A migration sentinel lets a downstream project detect, at CMake configure time,
that one specific ITK change is present — during the interval between ITK tags,
which can run two years.

The motivating case is `git bisect`. A downstream project bisecting ITK to find
which change broke it is configured against a sequence of arbitrary commits, and
at each step must choose which baseline images to test against. Sentinels are
plain files in the source tree, so they are readable in a detached `HEAD`, a
shallow clone, or an exported archive — states in which `git describe` fails and
`git rev-list --count` silently returns a wrong answer.

Sentinels are **not** a general feature-detection API. They are removed at each
tag. Sentinels are intended **only** for fine-grained configure-time decisions
between tagged releases.

## Adding a sentinel

Add one file to `CMake/MigrationSentinels/` containing a single line of
description:

```
CMake/MigrationSentinels/ITK_MIGRATION_PR6532.md
```

```markdown
itk::Math::SVD replaces vnl_svd for ITK-internal consumers; see PR #6532.
```

Choose exactly one naming pattern:

| Pattern | When |
|---|---|
| `ITK_MIGRATION_PR<N>` | **Strongly preferred** — traceable to the pull request. |
| `ITK_MIGRATION_<DESCRIPTIVE_NAME>` | No single PR captures the change. |
| `ITK_MIGRATION_HASH_<githash>` | Only a commit identifies the change. |

CI rejects a name matching none of these. It also rejects a misspelled prefix,
which would otherwise be silently ignored by the glob and never published.

## Using a sentinel downstream

```cmake
find_package(ITK REQUIRED)

if(ITK_VERSION VERSION_GREATER_EQUAL 6.1.0
   OR ITK_MIGRATION_PR6532 IN_LIST ITK_MIGRATION_SENTINELS)
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/post-6532)
else()
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/legacy)
endif()
```

Name the ITK release you expect to contain the change as the version arm. Both
arms are required:

- The **sentinel arm** is true between tags, before any release contains it.
- The **version arm** is true after the tag, once sentinels have been expired.

Against an ITK predating this mechanism, `ITK_MIGRATION_SENTINELS` is unset,
`IN_LIST` is false, and the legacy branch is taken — no guard needed. This is
also why the mechanism is a list variable rather than a function: calling a
function that older ITK does not define is a hard `Unknown CMake command`
error, and CMake's `if()` cannot invoke a function anyway.

## Expiring at a tag

Run once after tagging:

```bash
python3 Utilities/Maintenance/MigrationSentinels.py expire-itk
python3 Utilities/Maintenance/MigrationSentinels.py \
  expire-downstream --tag 6.1.0 --repo ../BRAINSTools
```

Order does not matter and neither step is urgent. A downstream not yet rewritten
keeps working against the tagged ITK because the version arm is now true.

## Why this is not in a compiled header

`ITK_MIGRATION_SENTINELS` is exported through `ITKConfig.cmake` only. Embedding
it in `itkConfigure.h` would change every translation unit's preprocessed output
on most merges and destroy ccache hit rates on every build machine. The
`MigrationSentinelConfigureHeaderGuard` test enforces this.
