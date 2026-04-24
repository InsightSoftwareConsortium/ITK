# Smoke Unit 10 — ImageRegionExclusionIteratorWithIndex / ImageRegionExclusionConstIteratorWithIndex

Reconnaissance for templating `ImageRegionExclusionIteratorWithIndex` on
a `bool VIsConst` non-type template parameter via a shared
`ImageRegionExclusionIteratorWithIndexBase<TImage, VIsConst>`, mirroring
the pattern established in `itkNeighborhoodIteratorBase.h` and applied
in Unit 4 (`ImageRegionIteratorWithIndex`).

Base commit: `0a8e45e164` (origin/smoke-base).
Branch: `smoke-unit-10-image-region-exclusion`.

## Summary

Both `Set(...)` and non-const `Value()` in
`itkImageRegionExclusionIteratorWithIndex.h` (lines 106 and 115) rely on
`const_cast<InternalPixelType *>(this->m_Position)` to strip the
`const` off the `m_Position` pointer inherited from the ultimate base
`ImageConstIteratorWithIndex`:

```
Modules/Core/Common/include/itkImageRegionExclusionIteratorWithIndex.h:106
    this->m_PixelAccessorFunctor.Set(*(const_cast<InternalPixelType *>(this->m_Position)), value);
Modules/Core/Common/include/itkImageRegionExclusionIteratorWithIndex.h:115
    return *(const_cast<InternalPixelType *>(this->m_Position));
```

Both casts disappear once `m_Position` is typed
`conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>`
at the member declaration, SFINAE-gating `Set` / non-const `Value`
on `!VIsConst`.

## Class structure (current)

```
ImageConstIterator<TImage>                     (owns m_Position: const InternalPixelType *)
  └─ ImageConstIteratorWithIndex<TImage>       (Unit 2 target)
       └─ ImageRegionConstIteratorWithIndex<TImage>           (Unit 4 target)
            └─ ImageRegionExclusionConstIteratorWithIndex<TImage>   (Unit 10 — this)
                 └─ ImageRegionExclusionIteratorWithIndex<TImage>   (Unit 10 — this)
                      – adds Set() + non-const Value() with const_cast
```

The Exclusion pair adds:
- `SetExclusionRegion(...)` / `SetExclusionRegionToInsetRegion()`
- overriden `operator++` / `operator--`, `GoToBegin`, `GoToReverseBegin`
- three private members: `m_ExclusionRegion`, `m_ExclusionBegin`, `m_ExclusionEnd`

None of the exclusion-specific logic interacts with the constness of
`m_Position`; the only `const_cast` call sites are in the non-const
child and serve the same role as in Units 2/4.

## Ripple: Unit 4 (and therefore Unit 2) MUST land first

This is the dominant architectural finding for Unit 10. The
`ImageRegionExclusionConstIteratorWithIndex` base class is
`ImageRegionConstIteratorWithIndex<TImage>` — exactly the class that
Unit 4 converts into an alias for
`ImageRegionIteratorWithIndexBase<TImage, /*VIsConst=*/true>`, which
in turn aliases into `ImageIteratorWithIndexBase<TImage, VIsConst>`
(Unit 2). Until those two templates exist, Unit 10 cannot inherit
from a `VIsConst`-parameterized base and therefore cannot eliminate
the `const_cast` at the source (the `m_Position` member type).

**Dependency chain: Unit 2 → Unit 4 → Unit 10.**

Once the ancestors are converted, Unit 10 collapses to a
near-mechanical rewrite:

1. Introduce `ImageRegionExclusionIteratorWithIndexBase<TImage, VIsConst>`
   inheriting from `ImageRegionIteratorWithIndexBase<TImage, VIsConst>`
   (Unit 4's new template).
2. Move the exclusion-specific state (`m_ExclusionRegion`,
   `m_ExclusionBegin`, `m_ExclusionEnd`) and methods
   (`SetExclusionRegion`, `SetExclusionRegionToInsetRegion`,
   `operator++`, `operator--`, `GoToBegin`, `GoToReverseBegin`)
   onto the base template; none of these depend on `VIsConst`.
3. SFINAE-gate `Set(...)` and non-const `Value()` on `!VIsConst`
   using the same idiom as Unit 4:

   ```cpp
   template <bool C = VIsConst, std::enable_if_t<!C, int> = 0>
   void Set(const PixelType & value) const
   {
     this->m_PixelAccessorFunctor.Set(*(this->m_Position), value);
   }
   template <bool C = VIsConst, std::enable_if_t<!C, int> = 0>
   PixelType & Value()
   {
     return *(this->m_Position);
   }
   ```

4. Provide alias templates retaining the legacy public names:

   ```cpp
   template <typename TImage>
   using ImageRegionExclusionConstIteratorWithIndex =
       ImageRegionExclusionIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

   template <typename TImage>
   using ImageRegionExclusionIteratorWithIndex =
       ImageRegionExclusionIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
   ```

5. Delete both `const_cast<InternalPixelType *>(...)` expressions.
   Each becomes `*(this->m_Position)` with the pointer already
   non-const by virtue of `conditional_t<VIsConst, ...>` in the
   ultimate base.

## const_cast grep (unit-10 target)

```
Modules/Core/Common/include/itkImageRegionExclusionIteratorWithIndex.h:106
Modules/Core/Common/include/itkImageRegionExclusionIteratorWithIndex.h:115
```

The const-iterator header (`itkImageRegionExclusionConstIteratorWithIndex.h`)
contains zero `const_cast`s — consistent with the pattern.

## External consumers

The `itkImageRegionExclusion*` names are used outside
`Modules/Core/Common/include/` (mechanical alias-template migration
should be transparent to all of them):

- `Modules/Filtering/MathematicalMorphology/include/itkGrayscaleGrindPeakImageFilter.hxx`
- `Modules/Filtering/MathematicalMorphology/include/itkGrayscaleFillholeImageFilter.hxx`
- `Modules/Filtering/ImageGrid/include/itkPadImageFilterBase.hxx`
- `Modules/Filtering/Path/include/itkPathConstIterator.h`
- `Modules/Filtering/Path/include/itkPathIterator.h`
- `Modules/Filtering/ImageFrequency/include/itkFrequency*ImageRegion*IteratorWithIndex.h`
  (8 headers — all inherit the plain iterator types, so an alias-template
  conversion is ABI-transparent at the consumer's using-declaration site)
- `Modules/Segmentation/Classifiers/include/itkScalarImageKmeansImageFilter.hxx`
- Tests: `itkImageRegionExclusionIteratorWithIndexTest.cxx`,
  `itkImageIteratorsGTest.cxx` (already exercises both const and
  non-const instantiations in the same typed-test suite, lines
  168/169, 202/203, 225 — a good proxy compile-only validation once
  the templated base lands)

## Pitfalls

- **CTAD deduction guides**: both headers ship CTAD guides. Unit 4's
  plan commits to *preserving* both guides and pointing them at the
  alias template; Unit 10 must follow suit. The const-iterator guide
  uses `std::remove_const_t<TImage>` in its return type — that subtlety
  must be preserved.
- **Private exclusion state**: Unit 10 is the first templated base in
  the chain that has to move *private* members onto the base. They are
  independent of `VIsConst`, but the moved `operator++` / `operator--`
  / `GoToBegin` / `GoToReverseBegin` bodies in the `.hxx` must be
  re-templated on `<TImage, VIsConst>` and the private members
  accessed via `this->`.
- **`ImageRegionExclusionIteratorWithIndex`'s protected cast
  constructor / operator=** (taking the Const form): after aliasing,
  the "const form" is `...Base<TImage, true>` and the "non-const form"
  is `...Base<TImage, false>`; the constructor becomes a
  non-template member on the `false` specialization path and needs
  to accept a `const ...Base<TImage, true> &`. This is the same
  pattern Unit 4 will have solved; follow its solution.
- **No Unit 10 GTest today**: `itkImageIteratorsGTest.cxx` already
  includes both class names in its typed-test lists, so once the
  base template lands that GTest is the compile-only proof-of-
  structure (no separate new GTest needed for Unit 10).

## Scope of this commit

Reconnaissance only. No source edits. A non-empty
`.devlocal/smoke/` file keeps the branch distinct from `smoke-base`
so the draft PR can carry this analysis.
