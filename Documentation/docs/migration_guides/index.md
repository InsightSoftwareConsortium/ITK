# Migration Guides

These migration guides explain how to update major versions of ITK, whic may contain breaking changes to the API.

## Fine-grained versioning between release tags

ITK's fourth (tweak) version component is the `YYYYMMDD` (UTC) date of the
most recent migration-inducing change on `main`. It exists so downstream
projects building against untagged ITK commits — for example during a
`git bisect` — can make configure-time decisions at day granularity between
release tags:

```cmake
find_package(ITK REQUIRED)
if(ITK_VERSION_FULL VERSION_GREATER_EQUAL 6.0.0.20260715)
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/post-change)
else()
  set(BASELINE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/legacy)
endif()
```

`ITK_VERSION_FULL` and `ITK_VERSION_TWEAK` are exported through
`ITKConfig.cmake`. Against an older ITK that predates the tweak component,
`ITK_VERSION_FULL` is unset and the comparison is false, so the legacy
branch is taken.

Policy: any change that requires a migration-guide entry or alters public
API **must** update `ITK_VERSION_TWEAK` in `CMake/itkVersion.cmake` to the
date the change lands (`Utilities/Maintenance/BumpVersionTweak.py` does
this); other significant changes **may** bump it. The value
is never reset — release tags supersede it, and version ordering makes it
expire naturally.

```{toctree}
:hidden:
:maxdepth: 3

itk_6_migration_guide
itk_5_migration_guide
joint_histogram_mutual_information_metric_correction
```
