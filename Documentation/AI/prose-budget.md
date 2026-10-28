# ITK Prose Budget for Committed Text

Contract for text that lands in ITK's git history or its pull-request
workflow: in-source comments, commit messages, and PR bodies.

Rules follow the tiers in [rule-tiers.md](./rule-tiers.md). The
**Project standard** sections bind every contributor and are what
reviewers, human or AI, cite. The **Agent defaults** section binds AI
agents only; reviewers do not cite it.

## Project standard

Committed text must be load-bearing for a future reader who has the
diff open. Text that adds nothing for that reader is a review defect.

### Lengths

| Artifact | Audience | Lifetime | Standard |
|---|---|---|---|
| In-source comment (`// …`) | future reader | forever | Short; explains a WHY the code cannot show |
| Commit subject | `git log --oneline` reader | forever | ≤ 78 chars (enforced by `kw-commit-msg.py`) |
| Commit message body | bisecter, archaeologist | forever | Short; extended rationale goes in the PR |
| PR visible summary (above first `<details>`) | reviewer at merge time | until merged | Readable at a glance |
| PR `<details>` blocks | reviewer who wants depth | until merged | Unbounded (hidden by default) |
| PR HTML comments (`<!-- … -->`) | automation, raw-body readers | survives merge | Unbounded (invisible) |

### The three-layer rule

Each artifact has one job. **Do not duplicate content across layers.**

| Layer | Job |
|---|---|
| Commit subject | **WHAT** changed, in scannable form |
| Diff itself | **HOW** the change is implemented |
| Commit body | **WHY** the change exists, only when not visible from the diff |

If a future reader can answer the question by reading the next
artifact down, prose at this layer is wasted bytes. Cut it.

### Not in committed text

| Content | Where | Why |
|---|---|---|
| AI tool / model identifiers, session IDs | Commits and source; PRs only in the `<details>` disclosure required by [attribution.md](./attribution.md) | The human committer is accountable for the change. |
| CDash / Azure DevOps / GitHub Actions URLs, `buildId=…`, ccache stats | All | The resource expires; the text remains as noise. Extract the fact instead. |
| Backstory — "first I tried X, then Y, then Z…" | Commits and source | The reader needs the final invariant, not the development journey. |
| Explanations of code that the diff already shows | All | Duplication. Rely on the diff. |
| References to deleted or prior code — "we used to do X, now Y" | In-source comments | The comment becomes false as the code evolves. A commit body may describe the behavior a fix changes. |
| Unsupported quality claims ("a robust implementation") | All | State the property concretely ("robust to NaN input") or omit it. |
| Restatement of the commit subject as the first body line | Commits | The subject is right above. |
| Issue / PR cross-references in commit subjects | Commits | Subjects stay scannable; references go in the body. |

### When a body or comment is needed

Body / comment content **must** be load-bearing for a future reader
who has the diff open:

- A non-obvious invariant the change preserves
- A constraint imposed by an external requirement (standard, hardware, ABI rule)
- The reason a non-default design choice was selected
- A pointer to a permanent reference (Insight Journal DOI, RFC number, persistent issue with durable analysis)

If the text would not change a future reader's behavior, omit it.

## Agent defaults

**Agent default.** Proxies for the standard above, tuned to the
verbosity observed in AI-generated text. Agents follow them unless the
directing human says otherwise; human contributors may deviate freely.

| Artifact | Agent default |
|---|---|
| In-source comment | None unless needed; at most 1 line (ColumnLimit from `.clang-format`) |
| Commit message body | ≤ 12 non-blank lines; trailers excluded |
| PR visible summary | ≤ 3 sentences, ≤ 250 chars |

Also avoid:

- Marketing words — "robust", "comprehensive", "elegant",
  "best-in-class", "production-ready". State the property instead.
- First-person narrative ("we", "I", "my") in commits and comments.
- Any mention of prior behavior in commit bodies unless the change
  fixes it.
- Documentation comments (`/** … */`, docstrings) added to code the
  change only touches in passing.

### Self-audit

Before staging any commit message, in-source comment, or PR body,
read every line and answer:

| Question | If yes → |
|---|---|
| Could this line be removed without changing future-reader understanding? | Remove it. |
| Does this line restate the diff? | Remove it. |
| Does this line restate the commit subject? | Remove it. |
| Does this line tell a story about how the change was developed? | Remove it. |
| Does this line cite a transient URL? | Remove it. |
| Does this line reference deleted or previous behavior? | Remove it. |
| Does this line use first-person narrative? | Remove it or rephrase as a statement about the code. |
| Could a well-named identifier replace this comment? | Rename, delete the comment. |

If anything survives, that is the body. If nothing survives, the
artifact needs no body.

When a reviewer cites a rule from this document on a PR, apply the
corresponding cure across the whole PR, not just at the citation point.

## Examples

### Commit body — too verbose

```
Replace the recurring PrintSelf boilerplate with
itk::print_helper::PrintNumericTrait. The helper takes the output
stream and indent as explicit parameters (no implicit capture from
caller scope), uses constexpr dispatch to skip the cast when
NumericTraits<T>::PrintType coincides with T (so std::complex
specialisations and every built-in scalar wider than char go
through the value's own stream insertion overload), and relies on
the cast only for char-family types whose PrintType is int.

This addresses prior reviewer feedback asking for a named template
instead of a macro, taking os and indent as parameters, etc.

Verified locally on macOS / clang and Ubuntu / gcc.
```

### Commit body — acceptable

```
Add a non-macro replacement for the recurring PrintSelf boilerplate.

Explicit os/indent parameters (no implicit capture). constexpr
dispatch elides the cast when NumericTraits<T>::PrintType == T,
so only char-family types take the casting branch.
```

### In-source comment — too verbose

```cpp
// Route the displacement field through a CastImageFilter (streaming-aware no-op)
// and a PipelineMonitorImageFilter so we can assert the warper actually streamed
// its displacement-field input. The image input is intentionally NOT routed
// through a similar chain: WarpImageFilter requests its full image input for any
// output region, so verifying streaming on the image input would always fail.
using VectorCasterType = itk::CastImageFilter<FieldType, FieldType>;
```

### In-source comment — acceptable (one short line, only the non-obvious WHY)

```cpp
// Streaming verified on displacement-field input only; WarpImageFilter requests the full image input.
using VectorCasterType = itk::CastImageFilter<FieldType, FieldType>;
```

### PR visible summary — too verbose

Multi-paragraph wall of text covering design rationale, AI
disclosure, test transcripts, and reviewer-context, with no
`<details>` collapsing. Reviewers read it as poor respect for their
time.

### PR visible summary — acceptable

```markdown
Add itk::print_helper::PrintNumericTrait and convert 513 call
sites in 221 files. Supersedes #NNNN.

<details>
<summary>Why a template instead of a macro</summary>
…
</details>

<details>
<summary>Local validation</summary>
…
</details>
```

## Where this applies

- `git commit -m "…"` and `git commit -F …`
- `gh pr create --body …`, `gh pr edit --body …`, `gh pr comment --body …`
- Inline review replies via the GitHub API
- New `// …` comments added or modified in source files

## Enforcement

- `Utilities/Hooks/kw-prose-budget.py` runs at commit-msg stage. It
  checks project-standard patterns for everyone, and the commit body
  cap and agent-default patterns when `ITK_AI_AGENT=1`. It is advisory
  (exit 0) unless `ITK_AI_AGENT=1` or `ITK_PROSE_BUDGET_HARD=1` is set.
- Reviewers remain the authoritative gate, and judge the project
  standard, not the agent-default proxies.
