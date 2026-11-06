#!/usr/bin/env python3
"""Validate, generate, and release ITK migration records.

Records are YAML files in Documentation/docs/migration_guides/records/itk<N>/.
This module is also the Sphinx extension that generates the guide pages.

  MigrationRecords.py validate [--root DIR]   advisory checks; exit 0 unless
                                              ITK_MIGRATION_RECORDS_STRICT=1
  MigrationRecords.py generate [--quiet]      write the git-ignored guide pages
  MigrationRecords.py template                regenerate _template.yaml
  MigrationRecords.py release VERSION         assign unlisted records to VERSION
"""

import argparse
import json
import os
import pathlib
import re
import sys
import textwrap

import yaml

REPO = pathlib.Path(__file__).resolve().parents[2]
DOCS = REPO / "Documentation" / "docs" / "migration_guides"
RECORDS = DOCS / "records"
GENERATED = DOCS / "generated"
SCHEMA_NAME = "migration-record.schema.json"
TEMPLATE_NAME = "_template.yaml"
MANIFEST_NAME = "releases.yaml"
GH = "https://github.com/InsightSoftwareConsortium/ITK"

NAME_RE = re.compile(r"^(\d{8})_([A-Z][A-Za-z0-9]{2,59})\.yaml$")
MAJOR_RE = re.compile(r"^itk(\d+)$")
IMPACT_RANK = {"high": 0, "medium": 1, "low": 2}
IMPACT_BADGE = {"high": "bdg-danger", "medium": "bdg-warning", "low": "bdg-secondary"}
MARKDOWN_FIELDS = ("summary", "details")
# Live, not commented, in the template so a copied template is a starting record.
TEMPLATE_LIVE = (
    "schema_version",
    "title",
    "summary",
    "categories",
    "environment",
    "impact",
    "pr",
)
NOT_PORTABLE = [
    (re.compile(r"^\s*(`{3,}|~{3,})\s*\{"), "MyST directive fence"),
    (re.compile(r"^\s*:{3,}\s*\{"), "MyST colon-fence directive"),
    (re.compile(r"\{[a-z][\w:-]*\}`"), "MyST role"),
    (re.compile(r"^\s*>\s*\[!\w+\]"), "GitHub alert"),
    (re.compile(r"~~[^~]+~~"), "strikethrough"),
    (re.compile(r"^\s*[-*]\s+\[[ xX]\]\s"), "task list"),
    (re.compile(r"(?<![\w/#`\[])#\d+\b"), "bare #123 reference (write a full link)"),
    (re.compile(r"(?<![\w.`/])@[A-Za-z][\w-]+"), "bare @user mention"),
]
FOLDED_RE = re.compile(r":\s*>[-+0-9]*\s*(#.*)?$")


class Finding:
    def __init__(self, level, path, message, field=None):
        self.level, self.path, self.message, self.field = level, path, message, field

    def line(self):
        if not self.field or not self.path.is_file():
            return 1
        for number, text in enumerate(
            self.path.read_text(encoding="utf-8").splitlines(), 1
        ):
            if re.match(rf"^{re.escape(self.field)}\s*:", text):
                return number
        return 1

    def __str__(self):
        where = f"{self.path.relative_to(REPO) if self.path.is_relative_to(REPO) else self.path}"
        field = f" {self.field}:" if self.field else ""
        return f"{self.level}: {where}:{self.line()}:{field} {self.message}"


def load_schema(root):
    path = root / SCHEMA_NAME
    return json.loads(
        (path if path.exists() else RECORDS / SCHEMA_NAME).read_text(encoding="utf-8")
    )


def major_dirs(root):
    return sorted(
        (p for p in root.iterdir() if p.is_dir() and MAJOR_RE.match(p.name)),
        key=lambda p: int(MAJOR_RE.match(p.name).group(1)),
    )


def _list(value):
    return value if isinstance(value, list) else []


def _impact(record):
    value = record.get("impact")
    return value if isinstance(value, str) else None


def descriptor_words(descriptor):
    return re.sub(r"(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Za-z])(?=[0-9])", " ", descriptor)


def read_manifest(path):
    if not path.exists():
        return {}
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    return data if isinstance(data, dict) else {}


def load_records(major_dir):
    """Return (records, problems): records render even when imperfect."""
    records, problems = [], []
    release_of = {}
    try:
        manifest = read_manifest(major_dir / MANIFEST_NAME)
        for release, ids in manifest.items():
            for rid in _list(ids):
                release_of[rid] = str(release)
    except yaml.YAMLError as err:
        problems.append(
            Finding("error", major_dir / MANIFEST_NAME, f"does not parse: {err}")
        )
    for path in sorted(major_dir.glob("*.yaml")):
        if path.name == MANIFEST_NAME:
            continue
        match = NAME_RE.match(path.name)
        try:
            data = yaml.safe_load(path.read_text(encoding="utf-8"))
        except yaml.YAMLError as err:
            problems.append(
                Finding(
                    "error",
                    path,
                    f"does not parse, so it is left out of the guide: {err}",
                )
            )
            continue
        if not isinstance(data, dict):
            data = {}
        descriptor = match.group(2) if match else path.stem
        record = dict(data)
        record.update(
            _id=f"{major_dir.name}/{descriptor}",
            _descriptor=descriptor,
            _file=path,
            _release=release_of.get(descriptor),
        )
        if not isinstance(record.get("title"), str):
            record["title"] = descriptor_words(descriptor)
        records.append(record)
    return records, problems


def validate(root=RECORDS):
    import jsonschema

    findings = []
    schema = load_schema(root)
    validator = jsonschema.Draft202012Validator(schema)
    order = list(schema["properties"])
    allowed_top = {SCHEMA_NAME, TEMPLATE_NAME, ".yamllint.yaml"}
    for entry in sorted(root.iterdir()):
        if entry.is_file() and entry.name not in allowed_top:
            findings.append(
                Finding(
                    "error",
                    entry,
                    "only itk<N>/ directories, the schema, and the template belong here",
                )
            )
        if entry.is_dir() and not MAJOR_RE.match(entry.name):
            findings.append(
                Finding(
                    "error", entry, "directories are named itk<N>, for example itk6"
                )
            )
    template = root / TEMPLATE_NAME
    if template.exists() and template.read_text(encoding="utf-8") != render_template(
        schema
    ):
        findings.append(
            Finding(
                "warning",
                template,
                "differs from the schema; run `MigrationRecords.py template`",
            )
        )
    for major_dir in major_dirs(root):
        records, problems = load_records(major_dir)
        findings += problems
        seen = {}
        for path in sorted(major_dir.iterdir()):
            if path.name == MANIFEST_NAME:
                continue
            match = NAME_RE.match(path.name)
            if not match:
                findings.append(
                    Finding(
                        "error",
                        path,
                        "name it YYYYMMDD_<Descriptor>.yaml with a CamelCase descriptor, for example 20261106_RequireSse2On32BitX86.yaml",
                    )
                )
                continue
            descriptor = match.group(2)
            if descriptor in seen:
                findings.append(
                    Finding(
                        "error",
                        path,
                        f"descriptor {descriptor} is also used by {seen[descriptor].name}; descriptors are the record IDs",
                    )
                )
            seen[descriptor] = path
            text = path.read_text(encoding="utf-8")
            for number, line in enumerate(text.splitlines(), 1):
                if FOLDED_RE.search(line) and not line.lstrip().startswith("#"):
                    findings.append(
                        Finding(
                            "error",
                            path,
                            f"line {number} uses a folded block (>); use a literal block (|) so line breaks are kept",
                        )
                    )
        for record in records:
            path = record["_file"]
            raw = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
            if not isinstance(raw, dict):
                findings.append(
                    Finding(
                        "error",
                        path,
                        "a record is a YAML mapping; {} is the smallest valid record",
                    )
                )
                continue
            for err in validator.iter_errors(raw):
                field = err.absolute_path[0] if err.absolute_path else None
                findings.append(
                    Finding(
                        "error",
                        path,
                        f"{err.message}; see the description of this field in the schema",
                        field,
                    )
                )
            if raw.get("pr") == 0:
                findings.append(
                    Finding(
                        "error",
                        path,
                        "pr is 0; set the PR number once the draft PR is open (-1 means no PR)",
                        "pr",
                    )
                )
            for field in ("title", "summary"):
                example = (schema["properties"][field].get("examples") or [None])[0]
                if raw.get(field) == example:
                    findings.append(
                        Finding(
                            "warning",
                            path,
                            f"{field} is still the template example; describe this change",
                            field,
                        )
                    )
            if "impact" not in raw and raw:
                findings.append(
                    Finding(
                        "warning",
                        path,
                        "add impact (high, medium, low) so readers can rank this change; see the rubric in the schema",
                        "impact",
                    )
                )
            categories = {c for c in _list(raw.get("categories")) if isinstance(c, str)}
            if (
                raw.get("impact") == "low"
                and categories & {"ResultsChange", "DefaultChanged"}
                and not raw.get("details")
            ):
                findings.append(
                    Finding(
                        "warning",
                        path,
                        "results or defaults change silently, which the rubric rates at least medium; raise impact or explain in details why few users notice",
                        "impact",
                    )
                )
            keys = [k for k in raw if k in order]
            if keys != sorted(keys, key=order.index):
                findings.append(
                    Finding(
                        "warning",
                        path,
                        f"keys read best in template order: {', '.join(sorted(keys, key=order.index))}",
                    )
                )
            texts = [(f, raw.get(f)) for f in MARKDOWN_FIELDS]
            texts += [
                ("actions", a)
                for a in _list(raw.get("actions"))
                if isinstance(raw.get("actions"), list)
            ]
            texts += [
                ("examples", e.get("note"))
                for e in _list(raw.get("examples"))
                if isinstance(e, dict)
            ]
            for field, value in texts:
                if not isinstance(value, str):
                    continue
                prose = re.sub(r"^```.*?^```", "", value, flags=re.S | re.M)
                prose = re.sub(r"`[^`\n]*`|\[[^\]\n]*\]\([^)\n]*\)", "", prose)
                for line in prose.splitlines():
                    for pattern, label in NOT_PORTABLE:
                        if pattern.search(line):
                            findings.append(
                                Finding(
                                    "error",
                                    path,
                                    f"{label} renders differently on GitHub and in the docs: {line.strip()[:60]}",
                                    field,
                                )
                            )
        manifest_path = major_dir / MANIFEST_NAME
        if manifest_path.exists():
            try:
                manifest = read_manifest(manifest_path)
            except yaml.YAMLError:
                manifest = {}
            listed = {}
            for release, ids in manifest.items():
                if not isinstance(release, str):
                    findings.append(
                        Finding(
                            "error",
                            manifest_path,
                            f'release {release!r} must be quoted, for example "6.0"',
                        )
                    )
                for rid in _list(ids):
                    if rid in listed:
                        findings.append(
                            Finding(
                                "error",
                                manifest_path,
                                f"{rid} is listed under both {listed[rid]} and {release}",
                            )
                        )
                    listed[rid] = release
                    if rid not in seen:
                        findings.append(
                            Finding(
                                "error",
                                manifest_path,
                                f"{rid} has no record file; remove it from {MANIFEST_NAME} if the record was deleted",
                            )
                        )
        findings += _yamllint(root, major_dir)
    return findings


def _yamllint(root, major_dir):
    try:
        from yamllint import linter
        from yamllint.config import YamlLintConfig
    except ImportError:
        return []
    config_path = RECORDS / ".yamllint.yaml"
    if not config_path.exists():
        return []
    config = YamlLintConfig(file=str(config_path))
    findings = []
    for path in sorted(major_dir.glob("*.yaml")):
        for problem in linter.run(path.read_text(encoding="utf-8"), config, str(path)):
            findings.append(
                Finding("warning", path, f"line {problem.line}: {problem.desc}")
            )
    return findings


def _badges(record):
    impact = _impact(record)
    badges = [
        (
            f"{{{IMPACT_BADGE[impact]}}}`{impact}`"
            if impact in IMPACT_BADGE
            else "{bdg-light}`impact not rated`"
        )
    ]
    badges += [
        f"{{bdg-info-line}}`{c}`"
        for c in _list(record.get("categories"))
        if isinstance(c, str)
    ]
    badges += [
        f"{{bdg-primary-line}}`{e}`"
        for e in _list(record.get("environment"))
        if isinstance(e, str)
    ]
    return " ".join(badges)


def _provenance(record):
    parts = []
    pr = record.get("pr")
    if isinstance(pr, int) and pr > 0:
        parts.append(f"[PR #{pr}]({GH}/pull/{pr})")
    for commit in _list(record.get("commits")):
        if isinstance(commit, str):
            parts.append(f"[`{commit[:11]}`]({GH}/commit/{commit})")
    for key, label in (
        ("deprecated_in", "Deprecated in"),
        ("removed_in", "Removed in"),
    ):
        if isinstance(record.get(key), str):
            parts.append(f"{label} {record[key]}")
    if isinstance(record.get("legacy_guard"), str):
        parts.append(f"Old form available until `{record['legacy_guard']}` is set")
    return " · ".join(parts)


def _actions(record):
    actions = [a for a in _list(record.get("actions")) if isinstance(a, str)]
    if not actions:
        return []
    lines = ["**What you need to do**", ""]
    for action in actions:
        first, *rest = action.strip().split("\n")
        lines.append(f"1. {first}")
        lines += [f"   {line}" if line.strip() else "" for line in rest]
    return lines + [""]


def _render_record(record, level=3):
    anchor = record["_id"].replace("/", "-")
    heading = "#" * level
    lines = [f"({anchor})=", f"{heading} {record['title']}", "", _badges(record), ""]
    if record["_release"] and level == 3:
        lines += [f"*First released in ITK {record['_release']}.*", ""]
    if isinstance(record.get("summary"), str):
        lines += [record["summary"].rstrip(), ""]
    high = record.get("impact") == "high"
    if high:
        lines += _actions(record)
    body = [] if high else _actions(record)
    for example in _list(record.get("examples")):
        if not isinstance(example, dict):
            continue
        language = example.get("language", "text")
        if isinstance(example.get("note"), str):
            body += [example["note"].rstrip(), ""]
        for side in ("before", "after"):
            if isinstance(example.get(side), str):
                body += [
                    f"*{side.title()}:*",
                    "",
                    f"````{language}",
                    example[side].rstrip(),
                    "````",
                    "",
                ]
    renames = [r for r in _list(record.get("renames")) if isinstance(r, dict)]
    if renames:
        body += (
            ["| Old | New |", "| --- | --- |"]
            + [f"| `{r.get('old', '')}` | `{r.get('new', '')}` |" for r in renames]
            + [""]
        )
    if isinstance(record.get("details"), str):
        body += [record["details"].rstrip(), ""]
    links = [u for u in _list(record.get("links")) if isinstance(u, str)]
    if links:
        body += ["**Links:** " + ", ".join(f"<{u}>" for u in links), ""]
    provenance = _provenance(record)
    if provenance:
        body += [provenance, ""]
    body += [
        f"Record: [`{record['_file'].name}`]({GH}/blob/main/{record['_file'].relative_to(REPO).as_posix()})",
        "",
    ]
    lines += (
        [":::::{dropdown} Details and examples", ":class-container: sd-mb-4", ""]
        + body
        + [":::::", ""]
    )
    return lines


def _version_key(version):
    match = re.match(r"^(\d+)\.(\d+)(?:\.(\d+))?(?:(a|b|rc)(\d+))?$", version)
    if not match:
        return (0, 0, 0, 0, 0)
    major, minor, patch, stage, number = match.groups()
    stage_rank = {"a": 0, "b": 1, "rc": 2, None: 3}[stage]
    return (int(major), int(minor), int(patch or 0), stage_rank, int(number or 0))


def _order(record):
    return (IMPACT_RANK.get(_impact(record), 3), record["_file"].name)


def render_guide(major_dir):
    major = MAJOR_RE.match(major_dir.name).group(1)
    records, _ = load_records(major_dir)
    releases = sorted(
        {r["_release"] for r in records if r["_release"]},
        key=_version_key,
        reverse=True,
    )
    groups = [("Unreleased (on `main`)", [r for r in records if not r["_release"]])]
    groups += [
        (f"ITK {release}", [r for r in records if r["_release"] == release])
        for release in releases
    ]
    source = (
        f"{GH}/tree/main/Documentation/docs/migration_guides/records/{major_dir.name}"
    )
    lines = [
        "<!--",
        "  ##################################################################",
        "  ##  AUTO-GENERATED FILE. DO NOT EDIT.                            ##",
        "  ##                                                              ##",
        f"  ##  Generated from Documentation/docs/migration_guides/records/{major_dir.name}/",
        "  ##  by Utilities/Maintenance/MigrationRecords.py generate.      ##",
        "  ##  Edit or add a record instead; see                           ##",
        "  ##  Documentation/AI/migration-records.md.                      ##",
        "  ##################################################################",
        "-->",
        "",
        f"# ITK {major} Migration Guide",
        "",
        f"Changes that downstream code may need to act on when moving to ITK {major}.",
        "The first part lists the changes that affect many projects and what to do",
        "about each; the second lists every change by release, newest first.",
        f"This page is generated from the [migration records]({source});",
        "to correct or add an entry, edit a record.",
        "",
        "## Most impactful changes",
        "",
    ]
    high = [
        r
        for _, group in groups
        for r in sorted(group, key=_order)
        if r.get("impact") == "high"
    ]
    if high:
        for record in high:
            lines += _render_record(record)
    else:
        lines += ["No change is rated high impact.", ""]
    lines += [
        "## All changes",
        "",
        "| Impact | Change | Release |",
        "| --- | --- | --- |",
    ]
    for _, group in groups:
        for record in sorted(group, key=_order):
            anchor = record["_id"].replace("/", "-")
            impact = (
                record.get("impact") if _impact(record) in IMPACT_RANK else "not rated"
            )
            lines.append(
                f"| {impact} | [{record['title']}](#{anchor}) | {record['_release'] or 'unreleased'} |"
            )
    lines.append("")
    for title, group in groups:
        if not group:
            continue
        lines += [f"### {title}", ""]
        listed_above = [
            r for r in sorted(group, key=_order) if r.get("impact") == "high"
        ]
        if listed_above:
            lines += ["High impact, described under Most impactful changes:", ""]
            lines += [
                f"- [{r['title']}](#{r['_id'].replace('/', '-')})" for r in listed_above
            ]
            lines.append("")
        for record in sorted(group, key=_order):
            if record.get("impact") != "high":
                lines += _render_record(record, level=4)
    return "\n".join(lines).rstrip() + "\n"


def generate(root=RECORDS, out_dir=GENERATED, quiet=False):
    out_dir.mkdir(parents=True, exist_ok=True)
    written = []
    for major_dir in major_dirs(root):
        out = (
            out_dir
            / f"itk_{MAJOR_RE.match(major_dir.name).group(1)}_migration_guide.md"
        )
        text = render_guide(major_dir)
        if not out.exists() or out.read_text(encoding="utf-8") != text:
            out.write_text(text, encoding="utf-8")
        written.append(out)
    if not quiet:
        for out in written:
            print(f"wrote {out.relative_to(REPO)}")
    return written


def _yaml_value(key, value):
    class Literal(yaml.SafeDumper):
        def increase_indent(self, flow=False, indentless=False):
            return super().increase_indent(flow, False)

    def represent_str(dumper, data):
        style = "|" if "\n" in data else None
        return dumper.represent_scalar("tag:yaml.org,2002:str", data, style=style)

    Literal.add_representer(str, represent_str)
    return yaml.dump(
        {key: value}, Dumper=Literal, sort_keys=False, allow_unicode=True, width=1000
    ).rstrip("\n")


def render_template(schema):
    lines = [
        "# yaml-language-server: $schema=../migration-record.schema.json",
        "# ITK migration record template. GENERATED from migration-record.schema.json",
        "# by `Utilities/Maintenance/MigrationRecords.py template`; do not edit by hand.",
        "#",
        "# Copy to records/itk<N>/YYYYMMDD_<Descriptor>.yaml, then:",
        "#   1. Replace the example values of the live (uncommented) fields.",
        "#   2. Uncomment each optional (commented) field that has a value, and",
        "#      fill it in. Keep a field commented while it is empty: an",
        "#      uncommented empty value is invalid.",
        "#   3. Before committing, removing unused fields and their comments is",
        "#      recommended. Leaving them in is allowed.",
        "# Every field is optional; {} is a valid record.",
        "# Guide: Documentation/AI/migration-records.md",
        "",
    ]
    for key, spec in schema["properties"].items():
        lines.append("")
        for paragraph in textwrap.wrap(spec.get("description", ""), 76):
            lines.append(f"# {paragraph}")
        example = (spec.get("examples") or [None])[0]
        if example is None:
            example = "" if spec.get("type") == "string" else []
        prefix = "" if key in TEMPLATE_LIVE else "# "
        for text in _yaml_value(key, example).splitlines():
            lines.append(f"{prefix}{text}")
    return "\n".join(lines) + "\n"


def write_template(root=RECORDS):
    (root / TEMPLATE_NAME).write_text(
        render_template(load_schema(root)), encoding="utf-8"
    )
    print(f"wrote {(root / TEMPLATE_NAME).relative_to(REPO)}")


def release(version, root=RECORDS):
    if _version_key(version) == (0, 0, 0, 0, 0):
        sys.exit(f"release: {version!r} is not a version such as 6.0, 6.1.2, or 6.0b03")
    major_dir = root / f"itk{version.split('.')[0]}"
    if not major_dir.is_dir():
        sys.exit(f"release: {major_dir.relative_to(REPO)} does not exist")
    manifest_path = major_dir / MANIFEST_NAME
    manifest = read_manifest(manifest_path)
    if version in manifest:
        sys.exit(f"release: {version} is already in {manifest_path.relative_to(REPO)}")
    listed = {rid for ids in manifest.values() for rid in _list(ids)}
    records, _ = load_records(major_dir)
    new = [r["_descriptor"] for r in records if r["_descriptor"] not in listed]
    if not new:
        print(f"release: no unreleased records in {major_dir.name}")
        return
    text = (
        manifest_path.read_text(encoding="utf-8")
        if manifest_path.exists()
        else "# Written by `MigrationRecords.py release`; maps each release to its record descriptors.\n"
    )
    text = text.rstrip("\n") + f'\n"{version}":\n' + "".join(f"  - {d}\n" for d in new)
    manifest_path.write_text(text, encoding="utf-8")
    print(
        f"release: assigned {len(new)} records to {version} in {manifest_path.relative_to(REPO)}"
    )


def report(findings, strict):
    annotate = os.environ.get("GITHUB_ACTIONS") == "true"
    for finding in findings:
        print(finding)
        if annotate:
            kind = "error" if strict and finding.level == "error" else "warning"
            rel = (
                finding.path.relative_to(REPO)
                if finding.path.is_relative_to(REPO)
                else finding.path
            )
            print(f"::{kind} file={rel},line={finding.line()}::{finding.message}")
    errors = sum(f.level == "error" for f in findings)
    if findings:
        mode = (
            "strict"
            if strict
            else "advisory; set ITK_MIGRATION_RECORDS_STRICT=1 to fail on errors"
        )
        print(f"{errors} error(s), {len(findings) - errors} warning(s) ({mode})")
    return 1 if strict and errors else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest="command", required=True)
    v = sub.add_parser("validate")
    v.add_argument("--root", type=pathlib.Path, default=RECORDS)
    g = sub.add_parser("generate")
    g.add_argument("--quiet", action="store_true")
    sub.add_parser("template")
    r = sub.add_parser("release")
    r.add_argument("version")
    args = parser.parse_args(argv)
    if args.command == "validate":
        strict = os.environ.get("ITK_MIGRATION_RECORDS_STRICT", "0") == "1"
        return report(validate(args.root.resolve()), strict)
    if args.command == "generate":
        generate(quiet=args.quiet)
    elif args.command == "template":
        write_template()
    elif args.command == "release":
        release(args.version)
    return 0


def _on_builder_inited(app):
    from sphinx.util import logging

    logger = logging.getLogger(__name__)
    for major_dir in major_dirs(RECORDS):
        _, problems = load_records(major_dir)
        for problem in problems:
            logger.warning(str(problem))
    try:
        imperfect = {f.path for f in validate() if f.level == "error"}
    except ImportError:
        imperfect = set()
    for path in sorted(imperfect):
        logger.warning(
            f"migration record {path.name} has validation errors; run MigrationRecords.py validate"
        )
    generate(quiet=True)


# Published URLs of the frozen guides, kept working after they leave the build.
OLD_PAGES = {
    "itk_6_migration_guide": "generated/itk_6_migration_guide.html",
    "itk_5_migration_guide": "generated/itk_5_migration_guide.html",
    "python_spatial_key_migration": "generated/itk_6_migration_guide.html#itk6-pythonspatialkeysorderexplicit",
    "joint_histogram_mutual_information_metric_correction": "generated/itk_6_migration_guide.html#itk6-jointhistogrammutualinformationmetriccorrected",
}


def _on_build_finished(app, exception):
    if exception is not None or app.builder.format != "html":
        return
    out_dir = pathlib.Path(app.outdir) / "migration_guides"
    out_dir.mkdir(parents=True, exist_ok=True)
    for page, target in OLD_PAGES.items():
        (out_dir / f"{page}.html").write_text(
            f'<!DOCTYPE html><html><head><meta charset="utf-8">'
            f'<meta http-equiv="refresh" content="0; url={target}">'
            f'<link rel="canonical" href="{target}"><title>Moved</title></head>'
            f'<body><p>This page moved to <a href="{target}">{target}</a>.</p></body></html>\n',
            encoding="utf-8",
        )


def setup(app):
    app.connect("builder-inited", _on_builder_inited)
    app.connect("build-finished", _on_build_finished)
    return {"parallel_read_safe": True, "parallel_write_safe": True}


if __name__ == "__main__":
    sys.exit(main())
