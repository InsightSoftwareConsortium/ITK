# Third-Party Fork Conventions

Several ITK third-party dependencies are vendored from a fork hosted under the
[InsightSoftwareConsortium](https://github.com/InsightSoftwareConsortium)
organization (for example
[eigen](https://github.com/InsightSoftwareConsortium/eigen) and
[DCMTK](https://github.com/InsightSoftwareConsortium/DCMTK)). Each fork carries
an overlay of ITK-specific patches on top of an upstream baseline.

This document defines how those forks are *named and structured*. For the
step-by-step procedure to update or initially port a dependency, see
[Updating Third Party Projects].

To keep these forks predictable and self-documenting, all ITK forks follow the
conventions below.

## Default branch: `welcome`

The default branch of every fork is an orphan branch named `welcome` containing
only a `README.md`. That README describes the upstream source, the branch
naming convention, and the step-by-step workflow for producing the next update.
Because GitHub renders the default branch's README on the repository landing
page, a visitor immediately sees how the fork is maintained instead of a stale
upstream description.

## Branch naming

ITK forks come in two types, and the overlay branch name's middle field tells
you which:

- **Release-anchored** — a thin overlay rebased onto a fixed upstream release:
  `for/itk-<project>-<version>-<shaN>`
  (e.g. `for/itk-eigen-5.0.1-bc3b39870`, `for/itk-dcmtk-3.7.0-ccfd10b`).
- **Branch-anchored** — an overlay tracking an upstream branch tip with no
  release to pin: `for/itk-<project>-<upstream-branch>-<shaN>`
  (e.g. `for/itk-vxl-master-272c3f1`).

where:

- `<project>` is the forked project (e.g. `eigen`, `dcmtk`, `vxl`),
- the middle field is a release `<version>` (e.g. `5.0.1`) for release-anchored,
  or an upstream `<branch>` name (e.g. `master`) for branch-anchored,
- `<shaN>` is the abbreviated commit hash (at least 7 characters) of the
  overlay's base: the upstream release-tag commit for release-anchored, or the
  overlay-tip commit for branch-anchored.

For example, `for/itk-dcmtk-3.7.0-ccfd10b` carries the ITK overlay on upstream
DCMTK tag `DCMTK-3.7.0` (commit `ccfd10b`), while `for/itk-vxl-master-272c3f1`
is the `vxl` overlay tip — a pruned numerics-only subset with no upstream
release to track. The fork's `welcome` README states which type it is. Older
non-conforming branches are retained for historical reference but are no longer
the update target.

### Anchor overlay commits with branches, never tags

Each overlay commit ITK may pin is anchored by a **branch**; do **not** also
create a tag with the same name. A ref that exists as both a branch and a tag
(e.g. a branch and a tag both named `for/itk-vxl-master-fd75e8b`) is ambiguous:
`git checkout`/`git fetch` of that name emits `warning: refname is ambiguous`
and may resolve the wrong object, breaking `update-third-party.bash`. Protect
each anchor branch against deletion and force-push so the pinned commit stays
reachable as the moving branch advances.

## Pinning the fork in ITK

Each module records the overlay ref it consumes in its update script:

- `eigen` records the overlay branch in `tag=` in
  `Modules/ThirdParty/Eigen3/UpdateFromUpstream.sh`.
- `DCMTK` pins an immutable commit via `DCMTK_GIT_TAG` in
  `Modules/ThirdParty/DCMTK/DCMTKGitTag.cmake`.

Either way the overlay branch name embeds the upstream version (or branch) and
base `<shaN>`, so an imported overlay's provenance is explicit. When updating,
the per-fork `welcome` branch is the authoritative, current description of the
procedure; follow it, then update the corresponding ref in ITK and refresh the
patch-list comment. The general update and porting steps are in
[Updating Third Party Projects].

[Updating Third Party Projects]: ../docs/contributing/updating_third_party.md
