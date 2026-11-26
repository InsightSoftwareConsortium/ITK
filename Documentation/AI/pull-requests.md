# ITK Pull Request Guidelines

## Opening a PR

**Agent default:** AI-agent-assisted PRs are opened in **Draft mode**
and stay there until every item below is satisfied.

## Ready-for-Review Checklist

Applies to every PR, however it was produced.

- [ ] All automated CI tests pass (Azure Pipelines, GitHub Actions).
- [ ] The implementation is correct, complete, and fully understood.
- [ ] The PR description accurately reflects the changes made.
- [ ] You can explain every changed line to a reviewer — you are accountable
      for all code in the PR, regardless of how it was generated.
- [ ] The diff has been reviewed for security issues: buffer overflows,
      deprecated APIs, injection vectors, license conflicts.
- [ ] A change downstream code may need to act on is described in a
      [migration record](./migration-records.md). Whether one is needed is a
      judgment call; CI never requires it.

## Updating a PR

Keep a rebase and review changes in separate force-pushes, so GitHub's
per-push compare shows only the PR's changes:

```bash
git rebase upstream/main
git push --force-with-lease          # rebase only
# apply review fixups, autosquash, pre-commit run --all-files
git push --force-with-lease          # changes only
```

Conflict resolutions belong in the rebase push. Either order works; never
combine both in one push.

## Replying to review comments

**Agent default.** Each reply posted on its own sends its own
notification, so answering many review comments on one PR spams every
reviewer and maintainer watching it. Batch the replies into one review
submission, so the PR produces one notification for the whole round
instead of one per comment.

Create a pending review with the GraphQL `addPullRequestReview`
mutation and no `event`, so it sends nothing yet. Attach each reply to
the thread it answers with `addPullRequestReviewComment`, setting
`inReplyTo` to the target comment's ID, then submit that review once
with `submitPullRequestReview`. A reply that does not need to sit in a
thread goes in the review body instead.

The rule covers `gh pr review --comment` and `gh pr comment` too: one
consolidated comment per round, not one per reply.

## AI Disclosure and Attribution

See [attribution.md](./attribution.md) for the full policy. Key points:

- **Commits are clean** — no AI tool names, model IDs, or tool-specific
  trailers in commit messages.
- **AI disclosure goes in the PR description** — inside a collapsed
  `<details>` block so it doesn't clutter the reviewer's first read.
- **No disclosure needed** for mechanical changes (reformat, rename,
  boilerplate).

## PR Description Format

Lead with a short visible summary (**Agent default:** 1-3 lines).
Sequester longer analysis, AI disclosure, test output, and background
context inside `<details>` blocks. See [attribution.md](./attribution.md) for examples.

## Visible-summary budget

PR-body standards, the agent-default caps, and good/bad examples are
defined in [prose-budget.md](./prose-budget.md). Read it before
authoring any PR body.

Reviewer-actionable call-outs ("Stacked on #NNNN, do not merge
until that lands") stay visible above the first `<details>`;
context-only material goes inside `<details>`.

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
