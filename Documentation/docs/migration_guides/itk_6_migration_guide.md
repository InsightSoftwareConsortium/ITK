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

Removed ITKv5 migration/maintenance scripts
-----------------------

The following scripts used for migrating to ITKv5 were removed from the ITKv6.

```sh
  CheckForOutdatedDefines.sh
  EnumPrintFunction.py
  Move_DISALLOW_COPY_to_public_section.cpp
  ReplaceITK_NULLPTRMacroNames.sh
  ReplaceITK_OVERRIDEMacroNames.sh
  ReplaceitkGetObjectMacro.sh
  UpdateAllC++Headers.sh
  UseNativeC++Syntax.sh
  misc-unused-parameters.sh
  modernize-loop-convert.sh
  modernize-pass-by-value.sh
  modernize-return-braced-init-list.sh
  modernize-use-auto.sh
  modernize-use-bool-literals.sh
  modernize-use-default-member-init.sh
  modernize-use-emplace.sh
  modernize-use-equals-default.sh
  modernize-use-equals-delete.sh
  modernize-use-nullptr.sh
  modernize-use-override.sh
  performance-general.sh
  prefer-type-alias-over-typedef.sh
  prefer_constexpr_for_const_literals.sh
  readability-container-size-empty.sh
  replaceClassWithTypename.py
  replace_itkStaticConstMacro.sh
  replace_vnl_math_XXX.sh
```

Accessing outdated ITKv5 migration scripts
------------------------------------------

```
git worktree add .../ITKv5.4 v5.4.0
ls ../ITKv5/Utilities/ITKv5Preparation
```

Class changes
-------------

The `Clone()` member function of `itk::PointSet` now does a "deep copy" of its
data, creating a new instance that has a copy of the points, the point data and
the region information properties of the original PointSet object. With previous
ITK versions, `PointSet::Clone()` did not copy any data. (It previously just
created a default-constructed PointSet object, like `PointSet::CreateAnother()`
does.)

For the sake of code readability, a new `CoordinateType` alias is added for
each nested `CoordRepType` alias. The old `CoordRepType` aliases will still be
available with ITK 6.0, but it is recommended to use `CoordinateType` instead.
The `CoordRepType` aliases will be removed when `ITK_FUTURE_LEGACY_REMOVE` is
enabled. Similarly, `InputCoordinateType`, `OutputCoordinateType`, and
`ImagePointCoordinateType` replace `InputCoordRepType`, `OutputCoordRepType`,
and `ImagePointCoordRepType`, respectively.


ITKVNLInstantiation library is removed
--------------------------------------

The usage of ITKVNLInstantiation library should be replaced directly with the
ITKVNL module. The ITKVNLInstantiation library was an empty library used for
compatibility and provided transitive linking to ITKVNL.


Prefer itk::AnatomicalOrientation over itk::SpatialOrientation
------------------------------------------------

The enumeration defined in `itk::SpatialOrientation`, including `itk::SpatialOrientation::CoordinateTerms`,
`itk::SpatialOrientation::CoordinateMajornessTerms`, and `itk::SpatialOrientation::ValidCoordinateOrientations` may be
deprecated in the future. These terms described an orientation of an axis in a coordinate system, by the "from" or
"negative" direction. For example `CoordinateTerms::ITK_COORDINATE_Right` describes an axis moving from the "Right" to
the "Left" side of the body.

This is the opposite of the DICOM `Patient Orientation (0020,0020)` tag.

The `itk::AnatomicalOrientation` class now represents the anatomical orientation. The class can provide a representation
of itself as an unambiguous enumeration, string and a matrix. It provides both a `PositiveEnum` and a `NegativeEnum` for
three letter descriptions of the anatomical orientation, which is ambiguous.

The recommended unambiguous way to define an anatomical orientation is the following:

```cpp
itk::AnatomicalOrientation(itk::AnatomicalOrientation::CoordinateEnum::RightToLeft,
                           itk::AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
                           itk::AnatomicalOrientation::CoordinateEnum::InferiorToSuperior);
```

The `itk::SpatialOrientation::ValidCoordinateOrientations` enumerations can be explicitly or implicitly converted to the
new `AnatomicalOrientation` object:

```cpp
itk::AnatomicalOrientation orientation = itk::SpatialOrientation::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI;
orientation = itk::AnatomicalOrientation(itk::SpatialOrientation::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;)
```

Implicit conversion is not available in ITK Python.  An error messaging like the following can occur:
```text
TypeError: in method 'itkOrientImageFilterID3ID3_SetDesiredCoordinateOrientation', argument 2 of type 'itkAnatomicalOrientation'
```

Explicitly converting to `AnatomicalOrientation` is required in ITK Python:
```python
reoriented_image = itk.orient_image_filter(
    image,
    use_image_direction=True,
    desired_coordinate_orientation=itk.AnatomicalOrientation(
        itk.SpatialOrientationEnums.ValidCoordinateOrientations_ITK_COORDINATE_ORIENTATION_RAS
    ),
)
```
