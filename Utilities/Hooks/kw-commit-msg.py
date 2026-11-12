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

import os
import re
import sys

from pathlib import Path


MAX_SUBJECT_LENGTH: int = 78
GIT_GENERATED_PREFIXES = ("Merge ", "Revert ", "fixup! ", "squash! ", "amend! ")
SCISSORS_PATTERN = re.compile(r"^\S+ -+ >8 -+$")


def die(message, commit_msg_path):
    print("commit-msg hook failure", file=sys.stderr)
    print("-----------------------", file=sys.stderr)
    print(message, file=sys.stderr)
    print("-----------------------", file=sys.stderr)
    print(
        f"""
To continue editing, run the command
  git commit -e -F "{commit_msg_path}"
(assuming your working directory is at the top).""",
        file=sys.stderr,
    )
    sys.exit(1)


def main():
    git_dir_path: Path = Path(os.environ.get("GIT_DIR", ".git")).resolve()
    commit_msg_path: Path = git_dir_path / "COMMIT_MSG"

    if len(sys.argv) < 2:
        die(f"Usage: {sys.argv[0]} <git_commit_message_file>", commit_msg_path)

    input_file: Path = Path(sys.argv[1])
    if not input_file.exists():
        die(
            f"Missing input_file {sys.argv[1]} for {sys.argv[0]} processing",
            commit_msg_path,
        )
    original_input_file_lines: list[str] = []
    with open(input_file) as f_in:
        original_input_file_lines = f_in.readlines()

    input_file_lines: list[str] = []
    for test_line in original_input_file_lines:
        # Drop everything from the scissors line, e.g. the diff of `git commit -v`.
        if SCISSORS_PATTERN.match(test_line.rstrip("\n")):
            break
        test_line = test_line.strip()
        is_empty_line_before_subject: bool = (
            len(input_file_lines) == 0 and len(test_line) == 0
        )
        if test_line.startswith("#") or is_empty_line_before_subject:
            continue
        input_file_lines.append(f"{test_line}\n")

    with open(commit_msg_path, "w") as f_out:
        f_out.writelines(input_file_lines)

    if not input_file_lines:
        sys.exit(0)  # Let git abort the commit for an empty message.
    subject_line: str = input_file_lines[0]

    if len(subject_line) < 8:
        die(
            f"The first line must be at least 8 characters:\n--------\n{subject_line}\n--------",
            commit_msg_path,
        )
    if len(subject_line) > MAX_SUBJECT_LENGTH and not subject_line.startswith(
        GIT_GENERATED_PREFIXES
    ):
        die(
            f"The first line may be at most {MAX_SUBJECT_LENGTH} characters:\n"
            + "-" * MAX_SUBJECT_LENGTH
            + f"\n{subject_line}\n"
            + "-" * MAX_SUBJECT_LENGTH,
            commit_msg_path,
        )
    if re.match(r"^[ \t]|[ \t]$", subject_line):
        die(
            f"The first line may not have leading or trailing space:\n[{subject_line}]",
            commit_msg_path,
        )
    if not subject_line.startswith(GIT_GENERATED_PREFIXES) and not re.match(
        r"^(BUG:|COMP:|DOC:|ENH:|PERF:|STYLE:|WIP:)\s", subject_line
    ):
        die(
            f"""Start ITK commit messages with a standard prefix (and a space):
  BUG:    - fix for runtime crash or incorrect result
  COMP:   - compiler error or warning fix
  DOC:    - documentation change
  ENH:    - new functionality
  PERF:   - performance improvement
  STYLE:  - no logic impact (indentation, comments)
  WIP:    - Work In Progress not ready for merge
To reference GitHub issue XXXX, add "Issue #XXXX" to the commit message.
If the issue addresses an open issue, add "Closes #XXXX" to the message.""",
            commit_msg_path,
        )
    if re.match(r"^BUG: [0-9]+\.", subject_line):
        die(
            f'Do not put a "." after the bug number:\n\n  {subject_line}',
            commit_msg_path,
        )
    del subject_line

    if len(input_file_lines) > 1:
        second_line: str = input_file_lines[
            1
        ].strip()  # Remove whitespace at beginning and end
        if len(second_line) == 0:
            input_file_lines[1] = "\n"  # Replace line with only newline
        else:
            die(
                f'The second line of the commit message must be empty:\n"{second_line}" with length {len(second_line)}',
                commit_msg_path,
            )
        del second_line


if __name__ == "__main__":
    main()
