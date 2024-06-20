ITK v6 Migration Guide
======================

This guide documents the changes required to migrate a code base
which uses ITK v5 to use ITK v6. The migration guide for transition
from v4 to v5 can be found [here](./itk_5_migration_guide.md).

Require modern C++ language feature use
---------------------------------------
Many backward compatible/ forward enabling compiler features are now required to be used.

Replace `ITKv5_CONST` with `const`

Remove support for ITKv4 interfaces
-----------------------------------

`ITKV4_COMPATIBILITY` is no longer a supported option.  Previously this option
was off by default.  Previously when enabled, the ITK API was modified to
provide support for ITKV4 functions.

Remove support for ITKv5
-----------------------------------

All contents of the Deprecated module were removed. This includes TreeContainer
and related classes; atomic primitives, mutexes and related classes which are
now part of C++ standard; specialized Vector filters - specialized versions are
no longer needed, as regular filters can work with vector images. For details, see
[ITKv5 Migration Guide](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Documentation/docs/migration_guides/itk_5_migration_guide.md).
