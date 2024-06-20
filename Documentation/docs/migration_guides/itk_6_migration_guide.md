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

Prefer standard CXX language features rather than ITK macros
-------------------------------------------------------------

Replace ITK aliases (left column) with CXX standard feature (right column)

```txt
ITK_FALLTHROUGH                   [[fallthrough]]
ITK_DELETE_FUNCTION               = delete
ITK_CONSTEXPR_FUNC                constexpr
ITK_CONSTEXPR_VAR                 constexpr

ITK_ALIGNAS(X)                    alignas(X)
ITK_ALIGNOF(X)                    alignof(X)
ITK_DEPRECATED                    [[deprecated]]
ITK_DEPRECATED_MSG(MSG)           [[deprecated(MSG)]]
ITK_CONSTEXPR                     constexpr
ITK_DELETED_FUNCTION              = delete
ITK_EXTERN_TEMPLATE               extern
ITK_FINAL                         final
ITK_NOEXCEPT                      noexcept
ITK_NOEXCEPT_EXPR(X)              noexcept(X)
ITK_NULLPTR                       nullptr
ITK_OVERRIDE                      override
ITK_STATIC_ASSERT(X)              static_assert(X, #X)
ITK_STATIC_ASSERT_MSG(X, MSG)     static_assert(X, MSG)
ITK_THREAD_LOCAL                  thread_local
```
