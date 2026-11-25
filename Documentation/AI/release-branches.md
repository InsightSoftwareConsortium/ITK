# Release Branches and Backports

ITK keeps every maintenance branch connected to `main` by merging forward.
The human procedure is in
[contributing/index.md](../docs/contributing/index.md#merge-a-bug-fix-for-a-patch-release)
and [Release.md](../Maintenance/Release.md#integrate-bug-fixes-in-the-release-branch);
this page states the rules and the checks. Tiers follow
[rule-tiers.md](./rule-tiers.md).

## Branches

| Branch | Holds |
|---|---|
| `main` | Development |
| `release` | The newest release and its next patch release |
| `release-X.X` | One older series, e.g. `release-5.4` for ITK 5 |

## Where a fix starts

| The fix is for | Branch from, and open the PR against |
|---|---|
| New development | `main` |
| The next patch of the newest release | `release` |
| An older series | that `release-X.X` |
| A fix already merged on `main` | `release` or `release-X.X`; cherry-pick the `main` commit onto the topic |

A cherry-picked fix appears twice in history after the merge forward. That is
expected.

## Merge forward

Every change on a maintenance branch reaches `main` by merge, never by
cherry-pick alone:

```
release-X.X  →  release  →  main      (each step git merge --no-ff)
```

- Each step is its own pull request, so each conflict resolution is reviewed
  on its own.
- A conflict where `main` already holds the fix in a newer form resolves to
  `main`'s version. After resolving, `git diff upstream/main HEAD` shows only
  what the maintenance branch adds (release notes, a version-specific
  change); anything else is a resolution error.
- List every conflict and its resolution in the merge PR body.
- Never rebase or force-push `release` or `release-X.X`.

After a patch release is tagged, merge forward so the tag is an ancestor of
`main`. Merging sooner keeps each merge small: conflicts accumulate while a
maintenance branch goes unmerged.

## Checking conformance

Each command prints nothing when the branches conform:

```bash
git fetch upstream --tags
git log --oneline --no-merges upstream/release..upstream/release-5.4
git log --oneline --no-merges upstream/main..upstream/release
git log --oneline --no-merges upstream/main..upstream/release-5.4
for t in $(git ls-remote --tags --refs upstream 'v5.4*' 'v6*' | sed 's#.*refs/tags/##'); do
  git merge-base --is-ancestor "$t" upstream/main || echo "$t"
done
```

Use upstream's tags, as above; a stale local tag gives a false report.

## Agent defaults

**Agent default:**

- Do not open a merge-forward PR unless a maintainer asks for that step;
  release managers run them around patch releases.
- A backport is not finished when its PR merges. Report in the PR body
  which merge-forward steps it still needs, using the commands above.
- Resolve a merge-forward conflict by taking the maintenance side only when
  `main` lacks the change; state the reason for each conflict.
