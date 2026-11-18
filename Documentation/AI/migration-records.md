# Migration Records

ITK's migration guides are generated from one YAML file per change, the
**migration record**. No PR edits a shared guide file, so records never
conflict. This page is the guide for contributors and agents alike.

The rules below come in the two tiers of [rule-tiers.md](./rule-tiers.md).
**Project standards** decide whether a record is acceptable, and any reviewer
may cite them. The **Agent default** section holds generation guidelines that
tell an agent how to write a record. They are not acceptance criteria.

## When a change needs a record

Add a record when a downstream project might have to change its code,
build settings, or test baselines, or needs to know about the change:

- removed, deprecated, or renamed public API
- changed results or defaults, including bug fixes that change output
- raised requirements: compiler, dependency, CMake, Python
- changed file-format behavior
- support policy

Internal refactors, test-only changes, and fixes with no observable change
need no record. Reviewers may suggest a record or adjust its impact; CI never
requires one.

## Writing a record

1. Open the draft PR.
2. Copy `Documentation/docs/migration_guides/records/_template.yaml` to
   `records/itk<N>/<YYYYMMDD>_<Descriptor>.yaml`, where `<N>` is the major
   version the change ships in, the date is today, and the descriptor is
   readable CamelCase (`RequireSse2On32BitX86`) unique within the directory.
3. Replace the example values of the live fields, set `pr` to the PR
   number, and uncomment each optional field that has a value. Removing
   unused commented fields before committing is recommended, not required.
   Every field is optional; `{}` is a valid record. The field definitions and the
   impact rubric are in
   [`migration-record.schema.json`](../docs/migration_guides/records/migration-record.schema.json)
   and appear as comments in the template.
4. Commit the record in the same commit as the change, so a revert,
   cherry-pick, or backport carries it.
5. Read the validator's advice from the pre-commit hook and improve the
   record.

`hints` is a freeform list for people and agentic tools carrying out the
migration, inside ITK or downstream: a script in `Utilities/Maintenance/`, a
`sed` or `grep` command, a search pattern. Hints are not shown in the
generated guide.

Add `commits` once the hashes are final on `main`, and append the commits of
follow-up fixes to the same record.

## Project standards

These decide whether a record is acceptable.

- Migration notes are written as records. The frozen
  `itk_*_migration_guide.md` files are not edited.
- A record stands on its own: a reader of the guide never sees the PR. It may
  reuse anything relevant from the PR body.
- Markdown in `summary`, `details`, `actions`, and example notes renders the
  same on GitHub and in the docs: CommonMark plus pipe tables with explicit
  links. No MyST directives or roles, no GitHub alerts, no bare `#123` or
  `@user`.
- YAML that parses as written: versions are quoted (`"6.0"`, because an
  unquoted `6.10` parses as `6.1`), multi-line text uses literal blocks
  (`|`) and never folded blocks (`>`), and unused keys are left out rather
  than left empty.
- A change that ships in two release series has one record, in the guide of
  the series where downstream code must act.

There is **no length limit**. A human-written record may be as long and
detailed as its author finds useful, and reviewers, human or AI, should not
flag its length.

## Agent default: generation guidelines

These apply when an agent writes or edits a record. They constrain how the
agent generates prose. They are **not** acceptance criteria, and reviewers,
including AI reviewers such as Greptile, should not cite them against a
record.

- When a diff meets the test above, draft a record and say in the PR body
  that one was added.
- Fill every field the diff supports: `title`, `summary`, `categories`,
  `environment`, `impact`, `actions`, `examples`, and `hints` where a script
  or command helps.
- Be brief. `title` is one line. `summary` is one to three sentences.
  `details` holds only what the reader needs beyond the summary and examples.
  Follow [prose-budget.md](./prose-budget.md).
- When converting existing text into a record, keep its words: organize it
  into fields, change markup only, and do not rewrite, condense, or correct
  it. The brevity guideline does not apply to converted text.
- If a rebase conflicts in a frozen `itk_*_migration_guide.md`, move your
  section into a record instead.

## Validation

Validation advises; it does not block. The `itk-migration-records-validate`
hook prints suggestions and exits 0. Setting
`ITK_MIGRATION_RECORDS_STRICT=1` makes errors fail, for anyone who opts in.
The validator checks the project standards, never length.

## Releases and the generated guide

Release membership lives in `records/itk<N>/releases.yaml`, written only by
`Utilities/Maintenance/MigrationRecords.py release <version>` when a release
is cut. Records not listed there appear under "Unreleased (on `main`)".

The guide pages are generated, never committed: the docs build (including
each PR's Read the Docs preview), `Utilities/SetupForDevelopment.sh`, and a
pre-commit hook write them to the git-ignored
`Documentation/docs/migration_guides/generated/`. Agents and scripts read
the YAML records and `releases.yaml` directly.
