#!/usr/bin/env python3

"""List all new authors since the given revision. Only tested on Linux."""

import os
import sys
import subprocess
from pathlib import Path

if len(sys.argv) < 2 or sys.argv[1] == "-h" or sys.argv[1] == "--help":
    usage = "Usage: authors-since.py revision"
    print(usage)
    sys.exit(1)
revision = sys.argv[1]


previous_authors = set()
recent_authors = set()
authors_with_email = set()


def remove_maintainers(authors):
    individual_authors = []
    for author in authors:
        if (
            author.lower().find("maintainer") == -1
            and author.lower().find("robot") == -1
            and author.lower().find("group") == -1
            and author.lower().find("upstream") == -1
            and len(author) > 3
        ):
            individual_authors.append(author)
    return set(individual_authors)


def decode_authors(git_output):
    return git_output.decode("utf-8").strip().split("\n")


def update_previous_authors(revision):
    previous_authors_output = subprocess.check_output(
        f'git log --pretty=format:"%aN" {revision} | sort -u', shell=True
    )
    previous_authors.update(decode_authors(previous_authors_output))


def update_recent_authors(revision):
    recent_authors_out = subprocess.check_output(
        f'git log --pretty=format:"%aN" {revision} | sort -u', shell=True
    )
    recent_authors.update(decode_authors(recent_authors_out))


def update_authors_with_email(revision):
    authors_with_email_out = subprocess.check_output(
        f'git log --pretty=format:"%aN <%aE>" {revision} | sort -u', shell=True
    )
    authors_with_email.update(decode_authors(authors_with_email_out))


def remote_repository(remote_spec):
    for line in remote_spec.split("\n"):
        split = line.split()
        try:
            tag_index = split.index("GIT_REPOSITORY")
            return split[tag_index + 1].replace("${git_protocol}", "https").strip()
        except ValueError:
            continue

    print("GIT_REPOSITORY not found!")
    print("\nRemote spec:")
    print(remote_spec)
    sys.exit(1)


def remote_tag(remote_spec):
    for line in remote_spec.split("\n"):
        split = line.split()
        try:
            tag_index = split.index("GIT_TAG")
            return split[tag_index + 1].strip()
        except ValueError:
            continue

    print("GIT_TAG not found!")
    print("\nRemote spec:")
    print(remote_spec)
    sys.exit(1)


scratch_dir = Path("/tmp/AuthorsChangesSince")
if not scratch_dir.exists():
    os.makedirs(scratch_dir)


def format_shortlog(log, commit_link_prefix):
    output = ""

    current_author = ""
    current_author_output = ""
    bug_fixes = []
    platform_fixes = []
    doc_updates = []
    enhancements = []
    performance_improvements = []
    style_changes = []
    misc_changes = []

    def formatted_current_author():
        current_author_output = ""
        if enhancements:
            current_author_output += "\n#### Enhancements\n\n"
            for line, commit in enhancements:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if performance_improvements:
            current_author_output += "\n#### Performance Improvements\n\n"
            for line, commit in performance_improvements:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if doc_updates:
            current_author_output += "\n#### Documentation Updates\n\n"
            for line, commit in doc_updates:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if platform_fixes:
            current_author_output += "\n#### Platform Fixes\n\n"
            for line, commit in platform_fixes:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if bug_fixes:
            current_author_output += "\n#### Bug Fixes\n\n"
            for line, commit in bug_fixes:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if style_changes:
            current_author_output += "\n#### Style Changes\n\n"
            for line, commit in style_changes:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )

        if misc_changes:
            current_author_output += "\n#### Miscellaneous Changes\n\n"
            for line, commit in misc_changes:
                current_author_output += f"- {line}"
                current_author_output += " ([{0}]({1}{0}))\n".format(
                    commit, commit_link_prefix
                )
        current_author_output += "\n\n"
        return current_author_output

    for line in log.split("\n"):
        if not line:
            # $ blank
            pass
        elif line[:3] != "   ":
            if current_author:
                output += formatted_current_author()
            current_author = line
            output += "### " + current_author + "\n"
            bug_fixes = []
            platform_fixes = []
            doc_updates = []
            enhancements = []
            performance_improvements = []
            style_changes = []
            misc_changes = []
        else:
            prefix = line.split(":")[0].strip()
            commit = line.split(":")[-1]
            description = line.partition(":")[2].rpartition(":")[0].strip()
            if prefix == "BUG":
                bug_fixes.append((description, commit))
            elif prefix == "COMP":
                platform_fixes.append((description, commit))
            elif prefix == "DOC":
                doc_updates.append((description, commit))
            elif prefix == "ENH":
                enhancements.append((description, commit))
            elif prefix == "PERF":
                performance_improvements.append((description, commit))
            elif prefix == "STYLE":
                style_changes.append((description, commit))
            else:
                description = line.rpartition(":")[0].strip()
                misc_changes.append((description, commit))
    output += formatted_current_author()

    return output


changelog_file = scratch_dir / "Changelog.md"
with open(changelog_file, "w") as fp:
    fp.write("")


def write_changelog(repo_name, commit_link_prefix, git_revision):
    log = subprocess.check_output(
        f"git shortlog --format=%s:%h --topo-order --no-merges {git_revision}",
        shell=True,
    ).decode("utf-8")
    formatted_log = format_shortlog(log, commit_link_prefix)
    with open(changelog_file, "a") as fp:
        fp.write(f"{repo_name} Changes Since {revision}\n")
        fp.write("---------------------------------------------\n\n")
        fp.write(formatted_log)
        fp.write("\n\n")


revision_time = subprocess.check_output(
    f'git show -s --format="%ci" {revision}^{{commit}}', shell=True
)
revision_time = revision_time.decode("utf-8").strip()
print("Revision time: " + revision_time + "\n")
itk_dir = Path(os.path.dirname(os.path.abspath(__file__))) / ".." / ".."

# ITK Repository
update_previous_authors(revision)
update_recent_authors(revision + "..")
update_authors_with_email(revision + "..")
commit_link_prefix = "https://github.com/InsightSoftwareConsortium/ITK/commit/"
write_changelog("ITK", commit_link_prefix, revision + "..")

# ITKSphinxExamples Repository
os.chdir(scratch_dir)
examples_dir = scratch_dir / "ITKSphinxExamples"
if not examples_dir.exists():
    subprocess.check_call(
        "git clone https://github.com/InsightSoftwareConsortium/ITKSphinxExamples",
        shell=True,
    )
os.chdir(examples_dir)
update_previous_authors(f'--until="{revision_time}"')
update_recent_authors(f'--since="{revision_time}"')
update_authors_with_email(f'--since="{revision_time}"')
commit_link_prefix = (
    "https://github.com/InsightSoftwareConsortium/ITKSphinxExamples/commit/"
)
write_changelog("ITK Sphinx Examples", commit_link_prefix, f'--since="{revision_time}"')

# ITKSoftwareGuide Repository
os.chdir(scratch_dir)
examples_dir = scratch_dir / "ITKSoftwareGuide"
if not examples_dir.exists():
    subprocess.check_call(
        "git clone https://github.com/InsightSoftwareConsortium/ITKSoftwareGuide",
        shell=True,
    )
os.chdir(examples_dir)
update_previous_authors(f'--until="{revision_time}"')
update_recent_authors(f'--since="{revision_time}"')
update_authors_with_email(f'--since="{revision_time}"')
commit_link_prefix = (
    "https://github.com/InsightSoftwareConsortium/ITKSoftwareGuide/commit/"
)
write_changelog("ITK Software Guide", commit_link_prefix, f'--since="{revision_time}"')

# Remote modules
os.chdir(itk_dir)
changed_remotes = (
    subprocess.check_output(
        f"git diff-index --diff-filter=AM --name-only {revision} -- Modules/Remote/",
        shell=True,
    )
    .decode("utf-8")
    .strip()
)
with open(changelog_file, "a") as fp:
    fp.write(f"Remote Module Changes Since {revision}\n")
    fp.write("---------------------------------------------\n\n")
print("Updated remote module:")
for remote in changed_remotes.split():
    module_name = remote.split("/")[-1].split(".")[0]
    if module_name in ["SphinxExamples", "CMakeLists", "README"]:
        continue
    sys.stdout.write(f"*{module_name}*, ")
    os.chdir(itk_dir)

    # The remote file could have been added or its name changed. Use the oldest
    # commit since revision with the current name.
    old_commit = (
        subprocess.check_output(f"git rev-list {revision}.. -- {remote}", shell=True)
        .decode("utf-8")
        .split()[-1]
    )
    try:
        remote_spec = subprocess.check_output(
            f"git show {old_commit}^:{remote}", shell=True
        ).decode("utf-8")
    except subprocess.CalledProcessError:
        remote_spec = subprocess.check_output(
            f"git show {old_commit}:{remote}", shell=True
        ).decode("utf-8")

    remote_old_tag = remote_tag(remote_spec)

    remote_spec = subprocess.check_output(f"git show HEAD:{remote}", shell=True).decode(
        "utf-8"
    )
    remote_new_tag = remote_tag(remote_spec)
    remote_repo = remote_repository(remote_spec)

    os.chdir(scratch_dir)
    remote_dir = scratch_dir / remote_repo.split("/")[-1]
    if not remote_dir.exists():
        subprocess.check_call(f"git clone {remote_repo} {remote_dir}", shell=True)
    os.chdir(remote_dir)
    # update_previous_authors('..{0}'.format(remote_new_tag))
    update_recent_authors(f"{remote_old_tag}..{remote_new_tag}")
    update_authors_with_email(f"{remote_old_tag}..{remote_new_tag}")

    try:
        log = subprocess.check_output(
            "git shortlog --format=%s:%h --topo-order --no-merges {}..{}".format(
                remote_old_tag, remote_new_tag
            ),
            shell=True,
        ).decode("utf-8")
    except:
        subprocess.CalledProcessError
        continue
    commit_link_prefix = remote_repo.replace(".git", "") + "/commit/"
    formatted_log = format_shortlog(log, commit_link_prefix)
    with open(changelog_file, "a") as fp:
        fp.write(f"## {module_name}:\n")
        fp.write(formatted_log)
        fp.write("\n")

os.chdir(scratch_dir)

recent_authors = remove_maintainers(recent_authors)
authors_with_email = remove_maintainers(authors_with_email)
previous_authors = remove_maintainers(previous_authors)
new_authors = recent_authors.difference(previous_authors)

# Print results
print("\n\nPrevious authors: " + str(len(previous_authors)))
print("Recent authors: " + str(len(recent_authors)))
print("\nNew authors: " + str(len(new_authors)))
with open(scratch_dir / "NewAuthors.txt", "w") as fp:
    for author in new_authors:
        sys.stdout.write(author + ", ")
        fp.write(author + ", ")

recent_authors_with_email = []
for author in authors_with_email:
    for an in recent_authors:
        if author.find(an) != -1:
            recent_authors_with_email.append(author)
print("\n\nRecent with emails:")
with open(scratch_dir / "RecentWithEmails.txt", "w") as fp:
    for author in recent_authors_with_email:
        sys.stdout.write(author + ", ")
        fp.write(author + ", ")

print("\n\nWith emails oneline:")
with open(scratch_dir / "WithEmailsOneline.txt", "w") as fp:
    for author in authors_with_email:
        sys.stdout.write(author + "\n")
        fp.write(author + "\n")

print("\n\nNames:")
with open(scratch_dir / "Names.txt", "w") as fp:
    for author in authors_with_email:
        sys.stdout.write(author.split("<")[0] + "\n")
        fp.write(author.split("<")[0] + "\n")

print("\n\nEmails:")
with open(scratch_dir / "Emails.txt", "w") as fp:
    for author in authors_with_email:
        sys.stdout.write(author.split("<")[1][:-1] + "\n")
        fp.write(author.split("<")[1][:-1] + "\n")
