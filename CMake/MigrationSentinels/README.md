# Migration sentinels

Each `ITK_MIGRATION_*.md` file in this directory asserts that one
downstream-visible change is present in this ITK source tree. The filenames are
collected into `ITK_MIGRATION_SENTINELS` at configure time and exported through
`ITKConfig.cmake`, so downstream projects can gate on a specific change during
the interval between ITK tags.

Add one file per downstream-visible change. See
`Documentation/docs/contributing/migration_sentinels.md`.

This file is not a sentinel: the glob matches only `ITK_MIGRATION_*.md`.
