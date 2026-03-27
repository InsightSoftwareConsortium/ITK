# ITK AI Agent Guide

ITK (Insight Toolkit) is a cross-platform, open-source C++ toolkit for
N-dimensional scientific image processing, segmentation, and registration.
Apache 2.0 licensed. Build tool: **Pixi** (wraps CMake + Ninja).

## Context on Demand

Load only what your task requires:

| Task | Read |
|------|------|
| Understanding the codebase layout | `Documentation/AI/architecture.md` |
| Building or configuring ITK | `Documentation/AI/building.md` |
| Writing or running tests | `Documentation/AI/testing.md` |
| Code style, commits, PRs | `Documentation/AI/style.md` |
| Writing ITK C++ or Python | `Documentation/AI/conventions.md` |

## AI-Generated Commits and Pull Requests

### Draft Pull Requests

Open AI-agent-assisted PRs in **Draft mode**. Do not convert to *Ready for Review* until:

- [ ] All automated CI tests pass.
- [ ] The implementation is correct, complete, and fully understood.
- [ ] The PR description accurately reflects the changes made.
- [ ] You can explain every line to a reviewer — you are accountable for all code in the PR.
- [ ] You have run relevant tests locally and confirmed they pass.
- [ ] You have reviewed for security issues (buffer overflows, deprecated APIs, etc.).

### Transparency Requirements

- State clearly in the PR description how AI tools generated or assisted with the code.
- Identify which portions are AI-generated and what modifications were made.
- Include evidence of local testing — do not rely on AI assertions of correctness.
- Commit messages must describe **what** changed and **why**, not merely that AI made the change.
- Follow ITK's commit format: `PREFIX: Description (≤78 chars)` — enforced by hook.
- Do not use a simple `Co-Authored-By: AI-Tool` tagline as the only disclosure.

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
