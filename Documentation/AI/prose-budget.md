# ITK Prose Budget for Committed Text

Single contract for every piece of text that lands in ITK's git
history or its pull-request workflow. Read before producing any
in-source comment, commit message, or PR body.

This document codifies recurring reviewer feedback. **Verbose
committed text is a review-blocking defect**, not a stylistic
preference.

## Caps

| Artifact | Audience | Lifetime | Hard cap |
|---|---|---|---|
| In-source comment (`// …`) | future reader | forever | 1 short line, ≤ 100 chars; default = none |
| Commit subject | bisecter / `git log --oneline` reader | forever | ≤ 78 chars (enforced by `kw-commit-msg.py`) |
| Commit message body | bisecter, archaeologist | forever | ≤ 12 lines, ≤ 72 chars/line; trailers excluded |
| PR visible summary (above first `<details>`) | reviewer at merge time | until merged | ≤ 3 sentences, ≤ 250 chars |
| PR `<details>` blocks | reviewer who wants depth | until merged | unbounded (hidden by default) |
| PR HTML comments (`<!-- … -->`) | future automation, raw-body readers | survives merge | unbounded (invisible) |

## The three-layer rule

Each artifact has one job. **Do not duplicate content across layers.**

| Layer | Job |
|---|---|
| Commit subject | **WHAT** changed, in scannable form |
| Diff itself | **HOW** the change is implemented |
| Commit body | **WHY** the change exists, only when not visible from the diff |

If a future reader can answer the question by reading the next
artifact down, prose at this layer is wasted bytes. Cut it.

## Forbidden in committed text

The following content is forbidden in commit messages, in-source
comments, and PR bodies — including inside `<details>` blocks. It
belongs in HTML-comment provenance, an external issue tracker, or a
session transcript no other reader needs.

| Forbidden | Why |
|---|---|
| AI tool / model identifiers, "Claude Code", "ChatGPT", "session 2026-…" | The producing agent is irrelevant; the human committer is accountable. |
| CDash / Azure DevOps / GitHub Actions URLs, `buildId=…`, ccache stats | These resources expire; the commit lives forever. The link rots; the prose remains as noise. |
| Backstory / transcript narratives — "first I tried X, then Y, then Z…" | The reader needs the final invariant, not the development journey. |
| Explanations of code that the diff already shows | Duplication. Rely on the diff. |
| References to deleted or prior behavior — "we used to do X, now Y" | `git blame` and `git log` already record this; the comment becomes a lie when surrounding code evolves. |
| First-person narrative ("we", "I", "my") | Commits are statements about the code, not the author. |
| Marketing language — "robust", "comprehensive", "elegant", "best-in-class", "production-ready" | Zero technical signal. State the property concretely instead. |
| Restatement of the commit subject as the first body line | The subject is right above; do not paraphrase it. |
| Issue / PR cross-references in commit subjects | Allowed in the body as a single trailer line. Subjects must remain scannable. |
| Documentation comments (`/** … */`, docstrings) added to drive-by-touched code | Out of scope for non-doc changes; doxygen drift is invisible to type-checkers. |

## When a body or comment is needed

Body / comment content **must** be load-bearing for a future reader
who has the diff open:

- A non-obvious invariant the change preserves
- A constraint imposed by an external requirement (standard, hardware, ABI rule)
- The reason a non-default design choice was selected
- A pointer to a permanent reference (Insight Journal DOI, RFC number, persistent issue with durable analysis)

If the text would not change a future reader's behavior, omit it.

## Self-audit

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

Every text-producing operation in the AI-assisted workflow:

- `git commit -m "…"` and `git commit -F …`
- `gh pr create --body …`, `gh pr edit --body …`, `gh pr comment --body …`
- Inline review replies via the GitHub API
- New `// …` comments added or modified in source files

The rule applies to humans too, but in practice the verbosity
problem is concentrated in AI-generated text. Agents must
self-audit against this document before producing any of the above.

## Enforcement

- `Utilities/Hooks/kw-prose-budget.py` runs at commit-msg stage and
  warns on body length, line width, and forbidden-content patterns.
  Default is advisory (exit 0); set `ITK_PROSE_BUDGET_HARD=1` to
  block.
- Reviewers remain the authoritative gate. The hook reduces
  round-trips by surfacing the most-cited rejection patterns at
  commit time.
- When a reviewer cites a rule from this document on a PR, treat
  the citation as a request to apply the corresponding cure
  tree-wide on the PR, not just at the citation point.
