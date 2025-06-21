#!/usr/bin/env python3
import os
import sys
import subprocess
import re

from pathlib import Path
import textwrap

DEFAULT_LINE_LENGTH: int = 78


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


def get_max_length():
    try:
        result = subprocess.run(
            ["git", "config", "--get", "hooks.commit-msg.ITKCommitSubjectMaxLength"],
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as _:
        return DEFAULT_LINE_LENGTH


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
    max_subjectline_length: int = get_max_length()

    original_input_file_lines: list[str] = []
    with open(input_file) as f_in:
        original_input_file_lines = f_in.readlines()

    input_file_lines: list[str] = []
    for test_line in original_input_file_lines:
        test_line = test_line.strip()
        is_empty_line_before_subject: bool = (
            len(input_file_lines) == 0 and len(test_line) == 0
        )
        if test_line.startswith("#") or is_empty_line_before_subject:
            continue
        input_file_lines.append(f"{test_line}\n")

    with open(commit_msg_path, "w") as f_out:
        f_out.writelines(input_file_lines)

    subject_line: str = input_file_lines[0]

    if len(subject_line) < 8:
        die(
            f"The first line must be at least 8 characters:\n--------\n{subject_line}\n--------",
            commit_msg_path,
        )
    if (
        len(subject_line) > max_subjectline_length
        and not subject_line.startswith("Merge ")
        and not subject_line.startswith("Revert ")
    ):
        die(
            f"The first line may be at most {max_subjectline_length} characters:\n"
            + "-" * max_subjectline_length
            + f"\n{subject_line}\n"
            + "-" * max_subjectline_length,
            commit_msg_path,
        )
    if re.match(r"^[ \t]|[ \t]$", subject_line):
        die(
            f"The first line may not have leading or trailing space:\n[{subject_line}]",
            commit_msg_path,
        )
    if not re.match(
        r"^(Merge|Revert|BUG:|COMP:|DOC:|ENH:|PERF:|STYLE:|WIP:)\s", subject_line
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
        ].strip()  # Remove whitespace at begining and end
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
