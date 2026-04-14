# Commit and PR Attribution Rules

## Co-Authored-By: reserved for humans

`Co-Authored-By:` is for human contributors whose intellectual contribution
materially shaped the commit. **Never use it for AI tools** — the human who
prompted, reviewed, and committed bears authorship responsibility.

Reviewer `Co-Authored-By:` is **encouraged** when a reviewer's feedback
materially shaped the code (e.g., "keep v142 but migrate to windows-2022").

**The test:** if this code has a bug, who gets paged? If the answer is a
human, that human may be a Co-Author. If the answer is "the AI," you have
a process problem.

## AI tool attribution

| Situation | Format |
|-----------|--------|
| Mechanical change (reformat, rename, boilerplate) | No mention |
| Minor contribution (conflict resolution, API adaptation) | `Assisted-by: Claude Code — brief description` |
| Substantive contribution (root-cause analysis, algorithm design) | `Tool-Assisted:` structured trailer (see below) |

**Structured trailer** for substantive AI contributions:

```
Tool-Assisted: Claude Code (claude-opus-4-6)
  Role: root-cause analysis
  Contribution: identified CCACHE_NODIRECT=1 as sole cause of
    0.02% hit rate by comparing ARM CI with Azure DevOps.
```

Both `Tool-Assisted:` and `Assisted-by:` are parsable via
`git log --format='%(trailers:key=Tool-Assisted)'`.

## External context

When Discourse threads, issues, or other sources informed a commit,
attribute in prose with stable links. Replace transient URLs (CI build
links, Azure DevOps runs, CDash build URLs) with extracted context:

```
# BAD — transient links that will expire:
See https://dev.azure.com/itkrobot/.../buildId=14265
See https://open.cdash.org/build/12345678

# GOOD — context preserved:
Based on discussion in https://discourse.itk.org/t/7745 (patches
for 5.4.6). Azure DevOps ITK.macOS.Python build failed with exit
code 255 (dashboard script treats warnings as fatal; CDash shows
0 compile errors and 0 test failures).
```

## PR-level AI disclosure

State clearly in the PR description how AI tools contributed:
- Identify which portions are AI-generated and what modifications were made
- Include evidence of local testing — do not rely on AI assertions of correctness
- A bare `Co-Authored-By: AI-Tool` trailer is **not** sufficient disclosure

## Brevity

Attribution must not balloon commit messages. If the `Tool-Assisted:`
trailer would push past readability, use `Assisted-by:` in the commit
and move detail to the PR body. See [#6055][attribution-issue] for
the full policy discussion.

[attribution-issue]: https://github.com/InsightSoftwareConsortium/ITK/issues/6055
