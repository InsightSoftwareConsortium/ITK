# ITK Pull Request Guidelines

## Opening a PR

All AI-agent-assisted PRs must be opened in **Draft mode**. Do not convert
to *Ready for Review* until every item below is satisfied.

## Ready-for-Review Checklist

- [ ] All automated CI tests pass (Azure Pipelines, GitHub Actions).
- [ ] The implementation is correct, complete, and fully understood.
- [ ] The PR description accurately reflects the changes made.
- [ ] You can explain every changed line to a reviewer — you are accountable
      for all code in the PR, regardless of how it was generated.
- [ ] The diff has been reviewed for security issues: buffer overflows,
      deprecated APIs, injection vectors, license conflicts.

## AI Disclosure and Attribution

See [attribution.md](./attribution.md) for the full policy. Key points:

- **Commits are clean** — no AI tool names, model IDs, or tool-specific
  trailers in commit messages.
- **AI disclosure goes in the PR description** — inside a collapsed
  `<details>` block so it doesn't clutter the reviewer's first read.
- **No disclosure needed** for mechanical changes (reformat, rename,
  boilerplate).

## PR Description Format

Lead with a **1-3 line visible summary**. Sequester longer analysis,
AI disclosure, test output, and background context inside `<details>`
blocks. See [attribution.md](./attribution.md) for examples.

```markdown
Short summary of what changed and why. Closes #NNNN.

<details>
<summary>Root cause / design rationale</summary>

Longer explanation here, hidden by default.

</details>

<details>
<summary>AI assistance</summary>

- Tool, role, what it contributed
- Evidence of local testing

</details>

<details>
<summary>Test results</summary>

Commands run, output, baseline comparisons.

</details>
```

## Commit Format

Each commit in the PR must follow ITK's required format. See
`Documentation/AI/git-commits.md` for the full specification.

## CI / CDash

- **Azure Pipelines** — Linux, Windows, macOS (C++ and Python builds)
- **GitHub Actions** — Pixi builds and Apple Silicon
- **CDash** — https://open.cdash.org/index.php?project=Insight
