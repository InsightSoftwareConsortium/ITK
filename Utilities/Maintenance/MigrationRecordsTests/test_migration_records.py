"""Fixture tests for Utilities/Maintenance/MigrationRecords.py."""

import importlib.util
import pathlib

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location(
    "MigrationRecords", HERE.parent / "MigrationRecords.py"
)
mr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mr)


def messages(root):
    return [
        (f.level, f.path.name, f.message) for f in mr.validate(HERE / "fixtures" / root)
    ]


def test_good_records_have_no_errors():
    assert [m for m in messages("good") if m[0] == "error"] == []


def test_empty_record_renders_with_derived_title():
    text = mr.render_guide(HERE / "fixtures" / "good" / "itk6")
    assert "AUTO-GENERATED FILE. DO NOT EDIT." in text
    assert "#### Nearly Empty" in text
    assert text.index("## Most impactful changes") < text.index("## All changes")
    assert "### Unreleased (on `main`)" in text and "### ITK 6.0b02" in text


def test_bad_records_report_each_problem():
    found = messages("bad")
    expect = {
        "20261106_BadImpactValue.yaml": "'moderate' is not one of",
        "20261106_PlaceholderPr.yaml": "pr is 0",
        "20261106_FoldedBlock.yaml": "folded block",
        "20261106_FoldedActionItem.yaml": "folded block",
        "20261106_NotAMapping.yaml": "YAML mapping",
        "20261106_badDescriptor.yaml": "CamelCase descriptor",
        "note.yml": "CamelCase descriptor",
        "releases.yaml": "must be quoted",
    }
    for name, fragment in expect.items():
        assert any(
            n == name and fragment in m and lvl == "error" for lvl, n, m in found
        ), (name, found)
    portable = [m for lvl, n, m in found if n == "20261106_NotPortableMarkdown.yaml"]
    assert any("GitHub alert" in m for m in portable)
    assert any("bare #123" in m for m in portable)
    assert any("strikethrough" in m for m in portable)
    myst = [m for lvl, n, m in found if n == "20261106_MystMarkupInCode.yaml"]
    assert any("MyST role" in m for m in myst)
    assert any("MyST directive fence" in m for m in myst)
    empty = [
        m for lvl, n, m in found if n == "20261106_EmptyValues.yaml" and lvl == "error"
    ]
    assert sum("does not match" in m for m in empty) == 2, empty
    assert any("non-empty" in m for m in empty), empty
    trailing = [
        m
        for lvl, n, m in found
        if n == "20261106_TrailingNewlineTitle.yaml" and lvl == "error"
    ]
    assert sum("does not match" in m for m in trailing) == 2, trailing
    assert any("non-empty" in m for m in trailing), trailing
    malformed = [
        m
        for lvl, n, m in found
        if n == "20261106_MalformedReferences.yaml" and lvl == "error"
    ]
    assert any("not-a-sha" in m for m in malformed), malformed
    assert any("www.example.com" in m for m in malformed), malformed
    assert sum("does not match" in m for m in malformed) == 3, malformed
    assert any("Missing has no record file" in m for lvl, n, m in found)


def test_advisory_by_default(monkeypatch):
    monkeypatch.delenv("ITK_MIGRATION_RECORDS_STRICT", raising=False)
    assert mr.main(["validate", "--root", str(HERE / "fixtures" / "bad")]) == 0
    monkeypatch.setenv("ITK_MIGRATION_RECORDS_STRICT", "1")
    assert mr.main(["validate", "--root", str(HERE / "fixtures" / "bad")]) == 1


def test_template_has_live_core_fields_and_unedited_copy_warns(tmp_path):
    schema = mr.load_schema(mr.RECORDS)
    template = mr.render_template(schema)
    live = mr.yaml.safe_load(template)
    assert set(live) == set(mr.TEMPLATE_LIVE)
    major = tmp_path / "itk6"
    major.mkdir()
    (major / "20261106_UneditedCopy.yaml").write_text(template)
    found = [(f.level, f.message) for f in mr.validate(tmp_path)]
    assert (
        "warning",
        "title is still the template example; describe this change",
    ) in found
    errors = [m for lvl, m in found if lvl == "error"]
    assert errors and all(m.startswith("pr is 0") for m in errors), errors


def test_wrong_field_types_do_not_crash():
    bad = HERE / "fixtures" / "bad"
    found = [(f.level, f.path.name, f.message) for f in mr.validate(bad)]
    assert any(
        n == "20261106_WrongFieldTypes.yaml" and lvl == "error" for lvl, n, m in found
    )
    assert "Wrong Field Types" in mr.render_guide(bad / "itk6")


def test_multiline_action_keeps_its_lines_under_the_step():
    text = mr.render_guide(HERE / "fixtures" / "good" / "itk6")
    assert (
        "1. Replace the old call.\n\n   ```cpp\n   filter->NewMethod();\n   ```\n1. Rebuild."
        in text
    )


def test_pipes_in_table_cells_are_escaped():
    text = mr.render_guide(HERE / "fixtures" / "good" / "itk6")
    assert "[Rename `operator\\|`]" in text
    assert "| `a\\|b` | `c` |" in text
