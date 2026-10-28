#!/usr/bin/env python3
# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================

"""Commit-message prose check.

Enforces the contract in Documentation/AI/prose-budget.md, using the
tiers in Documentation/AI/rule-tiers.md: project-standard patterns are
checked for everyone; the body cap and agent-default patterns only when
ITK_AI_AGENT=1. Advisory (exit 0) unless ITK_AI_AGENT=1 or
ITK_PROSE_BUDGET_HARD=1 is set.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path


BODY_LINE_CAP = 12

# Trailers (Co-Authored-By, Fixes, Refs, Signed-off-by, etc.) are
# allowed beyond the body cap and are not counted.
TRAILER_PATTERN = re.compile(
    r"^(?:Co-Authored-By|Signed-off-by|Fixes|Refs|Closes|"
    r"Reviewed-by|Acked-by|Tested-by|Reported-by|Suggested-by|"
    r"Cc|See-also|Tool-Assisted|Assisted-by):\s",
    re.IGNORECASE,
)

STANDARD_PATTERNS: list[tuple[re.Pattern[str], str]] = [
    (re.compile(r"https?://open\.cdash\.org\b", re.IGNORECASE), "transient CDash URL"),
    (
        re.compile(r"https?://dev\.azure\.com\b", re.IGNORECASE),
        "transient Azure DevOps URL",
    ),
    (
        re.compile(
            r"https?://github\.com/[^\s]*/(?:actions/runs|runs/\d)", re.IGNORECASE
        ),
        "transient GitHub Actions URL",
    ),
    (re.compile(r"\bbuildId\s*=\s*\d", re.IGNORECASE), "transient CI build ID"),
    (
        re.compile(
            r"\b(?:Claude\s*Code|ChatGPT|GPT-\d|Codex|Cursor|Copilot)\b", re.IGNORECASE
        ),
        "AI tool/model identifier",
    ),
    (
        re.compile(r"\bsession\s+\d{4}[-/]\d{1,2}[-/]\d{1,2}\b", re.IGNORECASE),
        "session timestamp",
    ),
]

AGENT_DEFAULT_PATTERNS: list[tuple[re.Pattern[str], str]] = [
    (
        re.compile(
            r"\b(?:robust|comprehensive|elegant|best[-\s]in[-\s]class|production[-\s]ready)\b",
            re.IGNORECASE,
        ),
        "marketing language",
    ),
    (
        re.compile(r"^\s*(?:we |I |my )", re.MULTILINE),
        "first-person narrative",
    ),
]

# A fix commit may describe the behavior it changes.
PRIOR_BEHAVIOR_PATTERN = (
    re.compile(r"\bused to (?:do|be|use)\b|\bpreviously (?:was|did)\b", re.IGNORECASE),
    "reference to deleted/prior behavior",
)
FIX_SUBJECT_PATTERN = re.compile(r"^(?:BUG|COMP):")


def warn(label: str, detail: str) -> None:
    print(f"prose-budget: {label}: {detail}", file=sys.stderr)


def read_message(commit_msg_path: Path) -> tuple[str, list[str]]:
    """Return the subject and the body lines of the commit message, with
    the leading blank line removed and any trailing trailers stripped."""
    raw = commit_msg_path.read_text(errors="replace")
    # Strip comment lines (git's '# ...' guidance lines and scissors).
    cleaned = "\n".join(
        line for line in raw.splitlines() if not line.lstrip().startswith("#")
    )
    parts = cleaned.split("\n", 2)
    if len(parts) < 3:
        return parts[0], []
    body = parts[2].rstrip("\n").splitlines()
    # Drop trailing trailers.
    while body and TRAILER_PATTERN.match(body[-1]):
        body.pop()
    while body and body[-1].strip() == "":
        body.pop()
    return parts[0], body


def check_budget(body: list[str]) -> int:
    findings = 0
    non_blank = [ln for ln in body if ln.strip()]
    if len(non_blank) > BODY_LINE_CAP:
        warn(
            "body too long",
            f"{len(non_blank)} non-blank body lines exceeds the "
            f"{BODY_LINE_CAP}-line cap (see Documentation/AI/prose-budget.md)",
        )
        findings += 1
    return findings


def check_forbidden(
    body: list[str], patterns: list[tuple[re.Pattern[str], str]]
) -> int:
    findings = 0
    text = "\n".join(body)
    for pattern, label in patterns:
        m = pattern.search(text)
        if m:
            warn(
                f"forbidden content ({label})",
                f"matched {m.group(0)!r} at offset {m.start()} "
                f"(see Documentation/AI/prose-budget.md)",
            )
            findings += 1
    return findings


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: kw-prose-budget.py <commit-msg-path>", file=sys.stderr)
        return 2
    subject, body = read_message(Path(argv[1]))
    if not body:
        return 0
    is_agent = os.environ.get("ITK_AI_AGENT", "0") == "1"
    findings = check_forbidden(body, STANDARD_PATTERNS)
    if is_agent:
        agent_patterns = list(AGENT_DEFAULT_PATTERNS)
        if not FIX_SUBJECT_PATTERN.match(subject):
            agent_patterns.append(PRIOR_BEHAVIOR_PATTERN)
        findings += check_budget(body)
        findings += check_forbidden(body, agent_patterns)
    if findings == 0:
        return 0
    if is_agent or os.environ.get("ITK_PROSE_BUDGET_HARD", "0") == "1":
        print(
            f"prose-budget: {findings} warning(s); rejecting commit "
            "(ITK_AI_AGENT=1 or ITK_PROSE_BUDGET_HARD=1).",
            file=sys.stderr,
        )
        return 1
    print(
        f"prose-budget: {findings} warning(s); commit allowed.",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
