# Smoke Unit 08 — ImageRandomIteratorWithIndex / ImageRandomConstIteratorWithIndex

Reconnaissance for templating `ImageRandomIteratorWithIndex` on a
`bool VIsConst` non-type template parameter via a shared
`ImageRandomIteratorWithIndexBase<TImage, VIsConst>`, mirroring the
pattern established in `itkNeighborhoodIteratorBase.h` (reference
pattern; path may be aspirational at time of writing).

Base commit: `0a8e45e164` (origin/smoke-base).
Branch: `smoke-unit-08-image-random-iterator-with-index`.

## Summary

Both `Set(...)` and non-const `Value()` in
`itkImageRandomIteratorWithIndex.h` (lines 105 and 114) rely on
`const_cast<InternalPixelType *>(this->m_Position)` to strip the
`const` from the inherited `m_Position` pointer. `m_Position` is
declared in the ultimate base `ImageConstIteratorWithIndex` as:

    const InternalPixelType * m_Position{ nullptr };

so the const_cast is structurally the same pattern tackled in
Units 1-4. The fix is the same: introduce a
`VIsConst`-parameterized base so `m_Position` is typed
`conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>`
at the member declaration, and SFINAE-gate `Set`/non-const `Value`
on `!VIsConst`. Legacy names become alias templates.

## const_cast grep (Unit 08 target)

```
Modules/Core/Common/include/itkImageRandomIteratorWithIndex.h:105
Modules/Core/Common/include/itkImageRandomIteratorWithIndex.h:114
```

The `*ConstIteratorWithIndex` header already carries no `const_cast`
(validated below). Both call sites live on `this->m_Position`,
inherited through:

    ImageRandomIteratorWithIndex<TImage>
      -> ImageRandomConstIteratorWithIndex<TImage>
        -> ImageConstIteratorWithIndex<TImage>   (owner of m_Position)

## Ripple: Parent chain must migrate first

This unit is structurally *downstream* of Unit 2. The `const_cast`
here is a symptom of the pointer typed in
`ImageConstIteratorWithIndex::m_Position`, not an independent hazard.

**Sequencing requirement (firm):** Unit 2
(`ImageIteratorWithIndex` / `ImageConstIteratorWithIndex`) MUST land
before Unit 8. Once the ultimate base becomes
`ImageIteratorWithIndexBase<TImage, VIsConst>` and its `m_Position`
field is `conditional_t`-typed, Unit 8 collapses to a mechanical
rewrite:

1. Introduce `ImageRandomIteratorWithIndexBase<TImage, VIsConst>`
   inheriting from `ImageIteratorWithIndexBase<TImage, VIsConst>`.
2. Move the random-jump state (`m_Generator`,
   `m_NumberOfSamplesRequested`, `m_NumberOfSamplesDone`,
   `m_NumberOfPixelsInRegion`) and the `GoToBegin/GoToEnd/IsAtBegin/
   IsAtEnd/operator++/operator--/SetNumberOfSamples/GetNumberOfSamples/
   ReinitializeSeed/RandomJump` API into the base template; none of
   these touch `m_Position`'s constness.
3. Define `Set` and non-const `Value` as SFINAE-gated
   (`template <bool C = VIsConst, std::enable_if_t<!C, int> = 0>`)
   member templates on the base.
4. Provide alias templates:

   ```cpp
   template <typename TImage>
   using ImageRandomConstIteratorWithIndex =
       ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

   template <typename TImage>
   using ImageRandomIteratorWithIndex =
       ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
   ```

5. Drop both `const_cast` call sites; the expressions become
   `*(this->m_Position)` with the correct non-const type.

## Pitfalls

- **CTAD deduction guides**: both headers ship CTAD guides
  (mutable: lines 128-130; const: lines 242-244). The const one uses
  `std::remove_const_t<TImage>` to collapse `Image<const T, N>` into
  `Image<T, N>`. Alias templates cannot carry deduction guides
  directly; provide guides on the base class template per the
  `itkNeighborhoodIteratorBase.h` convention and verify alias
  deduction in a compile-only test.
- **Protected cross-constness constructor + operator=**:
  `itkImageRandomIteratorWithIndex.h` lines 120-124 expose a
  protected constructor and `operator=` from
  `ImageRandomConstIteratorWithIndex<TImage>`. The const→non-const
  conversion gate must be preserved — standard fix is to define the
  cross-constness ctor on the base with
  `static_assert(!VIsConst, ...)` or an equivalent SFINAE gate, same
  strategy Unit 2 will employ on its superclass.
- **`m_PixelAccessorFunctor`**: Unit 8's `Set` uses
  `this->m_PixelAccessorFunctor.Set(...)`, inherited from
  `ImageConstIteratorWithIndex`. Const-safe; no block.
- **Out-of-class / .hxx definitions**: Random-jump state lives in
  `itkImageRandomConstIteratorWithIndex.hxx` (included at line 249).
  The constructor `ImageRandomConstIteratorWithIndex(const TImage *,
  const RegionType &)` and `ReinitializeSeed(...)`, `RandomJump()`
  are defined out-of-class. Template-parameter signatures change
  (now need `<TImage, VIsConst>`), so the .hxx must be updated in
  the same commit.
- **Mersenne Twister owned state**: `m_Generator` is a
  `Statistics::MersenneTwisterRandomVariateGenerator::Pointer`. Moving
  it into the base is trivial — it does not depend on `VIsConst`.
- **`RandomJump()` writes `m_PositionIndex` / `m_Position`**: the
  current const-variant `RandomJump` already writes to `m_Position`
  via the inherited member; because `m_Position` is typed `const
  InternalPixelType *` today, this assignment is legal (pointer
  *itself* is not const, only its pointee). Post-migration, the base
  stores a `conditional_t`-typed pointer and the assignment remains
  legal in both instantiations. No SFINAE gate needed on
  `RandomJump()`.
- **Downstream SuperBuild consumers**: BRAINSTools, Slicer, ANTs, and
  ITK remote modules include both headers. Alias templates preserve
  source compatibility; a spot-check of `Modules/` shows no
  friend-declarations of either class.

## Python wrapping

Both classes are marked `.notwrapped`:

- `Modules/Core/Common/wrapping/itkImageRandomIteratorWithIndex.notwrapped`
- `Modules/Core/Common/wrapping/itkImageRandomConstIteratorWithIndex.notwrapped`

Python wrapping is therefore a non-concern for Unit 8.

## Recommendation

**Defer Unit 8 landing until Unit 2 is merged.** Landing Unit 8
against an un-migrated Unit 2 would require either duplicating
`VIsConst` plumbing at two hierarchy layers or retaining the
`const_cast`s — both strictly worse than sequencing.

Post-Unit-2, Unit 8 should be:

- A <150-line diff across the two headers and one `.hxx`.
- Behavior-preserving: only type-level plumbing + SFINAE gating.
- Validated by `git grep const_cast Modules/Core/Common/include/itkImageRandom*WithIndex*`
  returning empty.

## Validation snapshot (current tree, this branch)

```
$ git grep -n const_cast \
    Modules/Core/Common/include/itkImageRandomIteratorWithIndex.h \
    Modules/Core/Common/include/itkImageRandomConstIteratorWithIndex.h
Modules/Core/Common/include/itkImageRandomIteratorWithIndex.h:105: ... const_cast<InternalPixelType *>(this->m_Position) ...
Modules/Core/Common/include/itkImageRandomIteratorWithIndex.h:114: return *(const_cast<InternalPixelType *>(this->m_Position));
```

The const header is already clean. Post-Unit-2 + Unit-8, both
should be match-free.
