# Rule Tiers: Project Standards and Agent Defaults

The files in this directory have three audiences: AI agents producing
changes, human contributors, and reviewers (human, and AI reviewers such
as Greptile). A rule written to correct a known AI writing habit helps
while an agent writes, but it is the wrong test for a human-authored
change. Every rule therefore belongs to one of two tiers.

## Project standard

Applies to every change, whoever wrote it. Reviewers, human or AI, may
cite it as a defect.

A standard states an outcome a reviewer can judge on any text:
"a comment explains a WHY the code cannot show", "committed text states
properties concretely". Compiler cautions, conventions, testing rules,
enforced style, and the commit format checked by `kw-commit-msg.py` are
project standards.

## Agent default

Applies when an AI agent produces the change. An agent default is a
proxy for a standard, tuned to a measured model tendency: word lists,
numeric caps, "no comment unless needed", mandatory self-audit steps,
and workflow gates such as opening PRs as drafts.

- Agents follow agent defaults unless the human directing them says
  otherwise for a specific change.
- Human contributors may follow them, and need no justification to
  deviate.
- Reviewers do not cite an agent default as a defect. When text that
  breaks an agent default also misses the standard behind it, cite the
  standard.

## Marking

Sections and table rows that are agent defaults carry the label
**Agent default**. Unlabeled content is a project standard.

When adding a rule, write the standard first. Add an agent default only
for an observed AI tendency, and name the standard it serves.

## Enforcement

| Tier | `kw-prose-budget.py` | Reviewers |
|---|---|---|
| Project standard | checked for everyone | may cite |
| Agent default | checked when `ITK_AI_AGENT=1` | do not cite |

The hook is advisory (exit 0) unless `ITK_AI_AGENT=1` or
`ITK_PROSE_BUDGET_HARD=1` is set; either makes every finding block the
commit. Agents set `ITK_AI_AGENT=1` in the environment of `git commit`.

`greptile.json` instructs Greptile to review against project standards
only.
