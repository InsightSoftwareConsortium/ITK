# Commit and PR Attribution Rules

## Commits: what and why only

Commit messages describe **what** changed and **why**. They do not
describe how the change was produced. AI tool details, model names, and
tool-specific trailers (`Tool-Assisted:`, `Assisted-by:`) do **not**
belong in commit messages.

Keep commit messages concise — a 1-line subject (≤ 78 chars) plus a
short body describing intent. Long rationale goes in the PR description,
not the commit body.

## Co-Authored-By

`Co-Authored-By:` is for contributors whose intellectual contribution
materially shaped the commit — typically a co-developer or a reviewer
whose feedback drove a design change.

**For AI tools: never.** The human who prompted, reviewed, and
committed the code bears authorship responsibility. `Co-Authored-By:`
implies accountability — AI tools cannot be paged when the code breaks,
cannot defend design decisions in review, and cannot maintain the code
going forward. AI assistance is disclosed in the PR description instead
(see below).

**For reviewers:** use `Co-Authored-By:` only when a reviewer's
feedback **materially shaped the design** of the commit (e.g., "keep
v142 but migrate to windows-2022" led to a different implementation
approach). A reviewer who caught a typo or requested a rename is a
reviewer, not a co-author. For review acknowledgment without
co-authorship, mention the reviewer in prose in the commit body or PR
description:

```
Addresses dzenanz's review: restored v142 toolset entry with updated
generator and runner image.
```

**The responsibility test:** if this code has a bug, who gets paged?
If the answer is a human, that human may be a Co-Author. If the answer
is "whoever committed it," the committer is the sole author.

## AI disclosure: PR description only

The PR description is the **sole location** for AI tool disclosure.
Commits are clean.

When AI tools made a substantive contribution (root-cause analysis,
algorithm design, hypothesis testing), disclose in the PR body:

```markdown
Short visible summary of what changed and why.

<details>
<summary>AI assistance</summary>

- Tool: Claude Code (claude-opus-4-6)
- Role: root-cause analysis of ccache hit rate regression
- Contribution: identified CCACHE_NODIRECT=1 as sole cause by
  comparing ARM CI with Azure DevOps pipeline configurations
- All code was reviewed and tested locally before committing

</details>
```

**No disclosure needed** for mechanical changes: reformatting, rename
refactoring, boilerplate generation, applying a well-known pattern the
human specified.

## PR body format: concise by default, details on request

PR descriptions must be **short and reviewer-friendly by default**.
A reviewer should understand the PR's purpose in under 10 seconds
from the visible text.

- **Lead with a 1-3 line summary** — what changed and why.
- **Sequester longer analysis inside `<details>` blocks** — root cause
  analysis, AI assistance details, test output, failed approaches,
  status tables, and dependency discussion.
- **Keep visible text to actionable items** — if the reviewer must read
  it to know what to do, keep it visible. If it's background context
  for those who want the deep dive, collapse it.

```markdown
Fix float-precision division in BresenhamLine::BuildLine. Closes #6054.

<details>
<summary>Root cause</summary>

The integer division `abs(dx)/abs(dy)` truncated to zero for
shallow angles, producing incorrect line segments...

</details>

<details>
<summary>AI assistance</summary>

Claude Code identified the truncation pattern and suggested the
`static_cast<double>` fix. Verified locally with the GTest suite.

</details>
```

## External context

When Discourse threads, issues, or other sources informed a commit,
attribute in prose with stable links. Replace transient URLs (CI build
links, Azure DevOps runs, CDash build URLs) with extracted context:

```
# BAD — transient links that will expire:
See https://dev.azure.com/itkrobot/.../buildId=14265

# GOOD — context preserved:
Based on discussion in https://discourse.itk.org/t/7745 (patches
for 5.4.6). Azure DevOps ITK.macOS.Python failed with exit code
255 (dashboard script treats warnings as fatal; CDash shows
0 compile errors and 0 test failures).
```

[attribution-issue]: https://github.com/InsightSoftwareConsortium/ITK/issues/6055
