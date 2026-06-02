# Third-Party Fork Conventions

Several ITK third-party dependencies are vendored from a fork hosted under the
[InsightSoftwareConsortium](https://github.com/InsightSoftwareConsortium)
organization (for example
[eigen](https://github.com/InsightSoftwareConsortium/eigen) and
[DCMTK](https://github.com/InsightSoftwareConsortium/DCMTK)). Each fork carries
a small overlay of ITK-specific patches on top of an upstream release.

To keep these forks predictable and self-documenting, all ITK forks follow one
convention.

## Default branch: `welcome`

The default branch of every fork is an orphan branch named `welcome` containing
only a `README.md`. That README describes the upstream source, the branch
naming convention, and the step-by-step workflow for producing the next update.
Because GitHub renders the default branch's README on the repository landing
page, a visitor immediately sees how the fork is maintained instead of a stale
upstream description.

## Branch naming: `for/itk-<project>-<version>-<sha7>`

Each overlay branch is named `for/itk-<project>-<version>-<sha7>` where:

- `<project>` is the forked project (e.g. `eigen`, `dcmtk`),
- `<version>` is the upstream release the overlay is built on (e.g. `3.7.0`),
- `<sha7>` is the 7-character commit hash of that upstream release tag.

For example, `for/itk-dcmtk-3.7.0-ccfd10b` carries the ITK overlay on top of
upstream DCMTK tag `DCMTK-3.7.0` (commit `ccfd10b`). Older non-conforming
branches are retained for historical reference but are no longer the update
target.

## Pinning the fork in ITK

ITK pins a specific overlay commit, not a branch name, so the build is
reproducible:

- `eigen` is consumed through `Modules/ThirdParty/Eigen3/UpdateFromUpstream.sh`.
- `DCMTK` is pinned by `DCMTK_GIT_TAG` in
  `Modules/ThirdParty/DCMTK/DCMTKGitTag.cmake`.

When updating, the per-fork `welcome` branch is the authoritative, current
description of the procedure; follow it, then update the corresponding pin in
ITK and refresh the patch-list comment.
