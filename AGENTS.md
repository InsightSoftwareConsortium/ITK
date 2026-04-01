# ITK AI Agent Guide

The Insight Toolkit (ITK) is a cross-platform, open-source C++ toolkit for
N-dimensional scientific image processing, segmentation, and registration.
Apache 2.0 licensed. Build tool: **Pixi** (wraps CMake + Ninja).

## Context to Load on Demand

Load only what your task requires:

| Task | Read |
|------|------|
| Understanding the codebase layout | [./Documentation/AI/architecture.md](./Documentation/AI/architecture.md) |
| Building or configuring ITK | [./Documentation/AI/building.md](./Documentation/AI/building.md) |
| Writing or running tests | [./Documentation/AI/testing.md](./Documentation/AI/testing.md) |
| Code style, naming conventions | [./Documentation/AI/style.md](./Documentation/AI/style.md) |
| Writing ITK C++ classes, CMake build configuration, and Python wrapping configuration | [./Documentation/AI/conventions.md](./Documentation/AI/conventions.md) |
| Creating a git commit with C++ changes | [./Documentation/AI/git-commits.md](./Documentation/AI/git-commits.md), [./Documentation/AI/compiler-cautions.md](./Documentation/AI/compiler-cautions.md), [./Documentation/AI/building.md](./Documentation/AI/building.md), [./Documentation/AI/testing.md](./Documentation/AI/testing.md) |
| Opening or updating a pull request | [./Documentation/AI/pull-requests.md](./Documentation/AI/pull-requests.md) |
| Avoiding compiler-specific pitfalls and refactoring hazards | [./Documentation/AI/compiler-cautions.md](./Documentation/AI/compiler-cautions.md) |

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
