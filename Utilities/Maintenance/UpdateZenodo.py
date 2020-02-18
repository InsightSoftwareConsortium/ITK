#!/usr/bin/env python3
"""Update and sort the creators list of the zenodo record."""
import git
import json
from subprocess import run, PIPE
from pathlib import Path
from fuzzywuzzy import fuzz, process

skip = {
    'KWSys Robot',
    'ITK Robot',
    'VXL Maintainers',
    'MetaIO Maintainers',
    'GDCM Upstream',
    'ITK Migration V4',
    'KWSys Upstream',
    'Eigen Upstream',
    'GCC-XML Upstream',
    'HDF Group',
    'HDF5 Maintainers',
    'Google double-conversion Maintainers',
    'GoogleTest Upstream',
    'KWIML Upstream',
    'LIBPNG Upstream',
    'Zlib Upstream',
    'ITK Developers',
    'ITK Maintainer',
    'Kitware SysAdmin',
    }


def decommify(name):
    return " ".join(name.split(", ")[::-1])

def commify(name):
    return ", ".join(name.split(" ", 1)[::-1])


if __name__ == "__main__":
    git_root = Path(git.Repo(".", search_parent_directories=True).working_dir)
    zenodo_file = git_root / ".zenodo.json"

    zenodo = json.loads(zenodo_file.read_text()) if zenodo_file.exists() else {}

    creator_map = {
        decommify(creator["name"]): creator for creator in zenodo.get("creators", [])
    }

    shortlog = run(["git", "shortlog", "-ns"], stdout=PIPE)
    commit_counts = dict(
        line.split("\t", 1)[::-1]
        for line in shortlog.stdout.decode().split("\n")
        if line
    )

    existing_creators = set(creator_map.keys())

    committers = []

    # Stable sort:
    # Number of commits in descending order
    # Ties broken by alphabetical order of first name
    for committer, count in sorted(commit_counts.items(), key=lambda x: (-int(x[1]), x[0])):
        # Require >= 10 commits
        if int(count) < 10:
            continue
        matches = process.extract(
            committer, creator_map.keys(), scorer=fuzz.token_sort_ratio, limit=2
        )
        match, score = matches[0]
        if score <= 80:
            if committer not in skip:
                print("No entry to sort:", committer)
            continue
        existing_creators.discard(match)
        committers.append(match)

    for unmatched in sorted(existing_creators):
        print("No matching commits:", unmatched)
        # Keep the entries to avoid removing people for bad matching
        committers.append(unmatched)

    creators = [
        creator_map.get(committer, {'name': commify(committer)})
        for committer in committers
        if committer not in skip
        ]

    zenodo["creators"] = creators

    zenodo_file.write_text("%s\n" % json.dumps(zenodo, indent=2))
