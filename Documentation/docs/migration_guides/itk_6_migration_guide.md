ITK v6 Migration Guide
======================

This guide documents the changes required to migrate a code base
which uses ITK v5 to use ITK v6. The migration guide for transition
from v4 to v5 can be found [here](./itk_5_migration_guide.md).

Legacy code removed
-------------------

Most code that was marked as legacy in ITK 5 has been removed, with exceptions
made on an as-needed basis. External code which previously required
`ITK_LEGACY_REMOVE` CMake option to be `OFF` in order to build will now fail
to compile. Before starting migration to v6 (as explained in this guide),
migration to v5 should be finished. Dependent code should build and pass tests
with `ITK_LEGACY_REMOVE` turned `ON` when compiling against
[ITK 5](https://github.com/InsightSoftwareConsortium/ITK/releases/tag/v5.4.0).

Most code which was marked as `ITK_FUTURE_LEGACY_REMOVE` has been now
re-flagged as `ITK_LEGACY_REMOVE`. There have been some other
deprecations and API changes. The new behavior is activated by setting
`ITK_LEGACY_REMOVE` to `ON`. By default, compatibility with v5 is retained
(`ITK_LEGACY_REMOVE=OFF`).

Remove support for ITKv4 interfaces
-----------------------------------

ITKV4_COMPATIBILITY is no longer a supported option.

Require modern C++ language feature use
---------------------------------------

Many backward compatible/ forward enabling compiler features are now required to be used.

* `ITKv5_CONST` must be replaced with `const`
* `ITK_ITERATOR_VIRTUAL` must be replaced with `virtual`
* `ITK_ITERATOR_OVERRIDE` must be replaced with `override`
* `ITK_ITERATOR_FINAL` must be replaced with `final`
* `ITK_DELETE_FUNCTION` must be replaced with `delete`