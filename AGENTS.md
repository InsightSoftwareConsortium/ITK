# ITK AI Agent Guide

The Insight Toolkit (ITK) is a cross-platform, open-source C++ toolkit for
N-dimensional scientific image processing, segmentation, and registration.
Apache 2.0 licensed. Build tool: **Pixi** (wraps CMake + Ninja).

## Context to Load on Demand

Load only what your task requires. Files are small and focused — load
the minimum set for the task at hand.

| Task | Read |
|------|------|
| Understanding the codebase layout | [architecture.md](./Documentation/AI/architecture.md) |
| Building or configuring ITK | [building.md](./Documentation/AI/building.md) |
| Writing or running tests | [testing.md](./Documentation/AI/testing.md) |
| Code style, formatting, naming | [enforced-code-style.md](./Documentation/AI/enforced-code-style.md) |
| Writing or reviewing C++ code | [code-review-lessons.md](./Documentation/AI/code-review-lessons.md) |
| Writing ITK C++ classes, CMake, Python wrapping | [conventions.md](./Documentation/AI/conventions.md) |
| Avoiding compiler pitfalls and refactoring hazards | [compiler-cautions.md](./Documentation/AI/compiler-cautions.md) |
| Creating a **DOC:** commit | [git-commits.md](./Documentation/AI/git-commits.md) |
| Creating a **STYLE:** commit | [git-commits.md](./Documentation/AI/git-commits.md), [enforced-code-style.md](./Documentation/AI/enforced-code-style.md) |
| Creating a **BUG:** or **ENH:** commit | [git-commits.md](./Documentation/AI/git-commits.md), [compiler-cautions.md](./Documentation/AI/compiler-cautions.md), [testing.md](./Documentation/AI/testing.md) |
| Creating a **COMP:** commit | [git-commits.md](./Documentation/AI/git-commits.md), [compiler-cautions.md](./Documentation/AI/compiler-cautions.md) |
| Commit or PR attribution | [attribution.md](./Documentation/AI/attribution.md) |
| Opening or updating a pull request | [pull-requests.md](./Documentation/AI/pull-requests.md), [attribution.md](./Documentation/AI/attribution.md) |

## Critical Pitfalls

1. **Template errors are verbose** — focus on the *first* error only.
2. **Python wrapping is incomplete** — check `wrapping/` dirs for available types before assuming a class is wrapped.
3. **Never `delete` ITK objects** — always use `SmartPointer` (`auto filter = FilterType::New()`).
4. **`Update()` is required** — filters don't execute until called; parameter changes after `Update()` need another call.
5. **Link errors → check `itk-module.cmake`** — missing `DEPENDS` or `PRIVATE_DEPENDS` is the usual cause.
6. **Licensing** — verify AI output does not reproduce third-party code in conflict with Apache 2.0.

## Resources

- Docs: https://docs.itk.org/
- Contribution guide: https://docs.itk.org/en/latest/contributing/
- Discourse: https://discourse.itk.org/
- Software Guide: https://itk.org/ItkSoftwareGuide.pdf
- Examples: https://examples.itk.org/
- Doxygen API: https://itk.org/Doxygen/html/
- CDash: https://open.cdash.org/index.php?project=Insight
