# Migration Guides

These migration guides explain how to update major versions of ITK, whic may contain breaking changes to the API.

The guides are generated from
[migration records](https://github.com/InsightSoftwareConsortium/ITK/blob/main/Documentation/AI/migration-records.md),
one YAML file per change.

## Fine-grained versioning between release tags

ITK's fourth (tweak) version component is the `YYYYMMDD` date of the most
recent commit, computed automatically at configure time. It exists so
downstream projects building against untagged ITK commits — for example during
a `git bisect` — can make configure-time decisions at day granularity between
release tags:

```cmake
find_package(ITK REQUIRED)
if(ITK_VERSION_FULL VERSION_GREATER_EQUAL 6.0.0.20260722)
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/post-change)
else()
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/legacy)
endif()
```

`ITK_VERSION_FULL` and `ITK_VERSION_TWEAK` are exported through
`ITKConfig.cmake`. Against an older ITK that predates the tweak component,
`ITK_VERSION_FULL` is unset and the comparison is false, so the legacy branch
is taken.

The date comes from `git log` in a working tree — including a shallow or
tagless clone — and from the `git archive`-substituted `.git_archival.txt` in
an exported source tarball; it falls back to `0` only when neither git history
nor that substitution is available. No manual edits are needed — the tweak
advances with every commit.

```{toctree}
:hidden:
:maxdepth: 3

generated/itk_6_migration_guide
generated/itk_5_migration_guide
```
