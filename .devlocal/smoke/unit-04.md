# Smoke Unit 04 — ImageRegionIteratorWithIndex / ImageRegionConstIteratorWithIndex

Reconnaissance for templating `ImageRegionIteratorWithIndex` on a
`bool VIsConst` non-type template parameter via a shared
`ImageRegionIteratorWithIndexBase<TImage, VIsConst>`, mirroring the
pattern established in `itkNeighborhoodIteratorBase.h`.

Base commit: `0a8e45e164` (origin/smoke-base).
Branch: `smoke-unit-04-image-region-iterator-with-index`.

## Summary

Both `Set(...)` and non-const `Value()` in
`itkImageRegionIteratorWithIndex.h` (lines 111 and 120) rely on
`const_cast<InternalPixelType *>(this->m_Position)` to strip the
`const` from the inherited `m_Position` pointer. `m_Position` is
declared in the ultimate base `ImageConstIteratorWithIndex` as:

    const InternalPixelType * m_Position{ nullptr };

so the const_cast is structurally the same pattern tackled in
Units 1-3. The fix is the same: introduce a
`VIsConst`-parameterized base so `m_Position` is typed
`conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>`
at the member declaration, and SFINAE-gate `Set`/non-const `Value`
on `!VIsConst`. Legacy names become alias templates.

## const_cast grep (ITK core iterator family)

```
Modules/Core/Common/include/itkImageIteratorWithIndex.h:112        (Unit 2)
Modules/Core/Common/include/itkImageIteratorWithIndex.h:121        (Unit 2)
Modules/Core/Common/include/itkImageRegionIteratorWithIndex.h:111  (Unit 4 target)
Modules/Core/Common/include/itkImageRegionIteratorWithIndex.h:120  (Unit 4 target)
```

Both Unit-4 const_casts live on `this->m_Position`, which is inherited
through two layers: `ImageRegionConstIteratorWithIndex<TImage>` →
`ImageConstIteratorWithIndex<TImage>` (pointer owner).

## Ripple: Parent must migrate first

This is the dominant architectural finding. `ImageRegionIteratorWithIndex`
is structurally a *thin* subclass — it adds only `operator++/--`
overrides (in the const parent) plus `Set/Value` (in the mutable
child). The `const_cast` here is not an independent hazard; it is a
symptom of the parent's `const InternalPixelType * m_Position` member.

**Unit 2 (`ImageIteratorWithIndex` / `ImageConstIteratorWithIndex`)
MUST land first.** Once the parent becomes
`ImageIteratorWithIndexBase<TImage, VIsConst>` and its `m_Position`
field is `conditional_t`-typed, Unit 4 collapses to a near-mechanical
rewrite:

1. Introduce `ImageRegionIteratorWithIndexBase<TImage, VIsConst>`
   inheriting from `ImageIteratorWithIndexBase<TImage, VIsConst>`.
2. Move `operator++()` / `operator--()` bodies (currently in
   `itkImageRegionConstIteratorWithIndex.hxx`) into the base template;
   they do not touch `m_Position`'s constness.
3. Define `Set` and non-const `Value` as SFINAE-gated
   (`template <bool C = VIsConst, std::enable_if_t<!C, int> = 0>`)
   member templates on the base.
4. Provide alias templates:

   ```cpp
   template <typename TImage>
   using ImageRegionConstIteratorWithIndex =
       ImageRegionIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

   template <typename TImage>
   using ImageRegionIteratorWithIndex =
       ImageRegionIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
   ```

5. Drop both `const_cast` call sites; the expression becomes
   `*(this->m_Position)` with the correct non-const type.

## Pitfalls

- **CTAD deduction guides**: both headers ship CTAD guides
  (lines 134-136 in the mutable header; 199-201 in the const header).
  The const one uses `std::remove_const_t<TImage>` to collapse
  `Image<const T, N>` into `Image<T, N>`. Alias templates cannot carry
  deduction guides directly; the replacement pattern used in
  `itkNeighborhoodIteratorBase.h` is to provide guides on the base
  class template and verify that the alias forms still deduce
  correctly. Worth a targeted check during Unit 4 landing.
- **Protected-for-const-correctness constructor**:
  `itkImageRegionIteratorWithIndex.h` lines 124-130 expose a
  *protected* constructor and assignment operator from
  `ImageRegionConstIteratorWithIndex<TImage>`. The base-class approach
  must preserve the const→non-const conversion gate. The standard fix
  is to define the cross-constness ctor on the base with
  `static_assert(!VIsConst, ...)` or equivalent SFINAE; this matches
  what Unit 2 will need for the same reason on its superclass.
- **`m_PixelAccessorFunctor` vs `m_PixelAccessor`**: Unit 4 uses
  `this->m_PixelAccessorFunctor` (inherited from
  `ImageConstIteratorWithIndex`), whereas Unit 2's parent
  `ImageIteratorWithIndex` uses `this->m_PixelAccessor`. Both are
  `const`-safe members; they do not themselves block the migration,
  but the Unit-2 refactor must keep both names live.
- **Out-of-class method definitions**: `operator++/--` are defined in
  `itkImageRegionConstIteratorWithIndex.hxx`. Template-parameter
  signatures change (now need `<TImage, VIsConst>`), so the .hxx must
  be updated in the same commit.
- **Downstream SuperBuild consumers**: BRAINSTools, Slicer, ANTs, and
  all ITK remote modules include
  `itkImageRegionIteratorWithIndex.h` ubiquitously. Alias templates
  preserve source compatibility; any unqualified friend-declaration
  of the class in third-party code could break, but a quick scan of
  ITK's own sources shows no such friend declarations in
  `Modules/`.

## Python wrapping

Both classes are wrapped. See:

- `Modules/Core/Common/wrapping/itkImageRegionIteratorWithIndex.wrap`
- `Modules/Core/Common/wrapping/itkImageRegionConstIteratorWithIndex.wrap`

Alias templates should preserve existing typedef names in the
generated `.i` files, so wrapping should remain unaffected. Worth
re-running `WrapITK` locally once Unit 2 + Unit 4 land together.

## Recommendation

**Defer Unit 4 landing until Unit 2 is merged.** The `const_cast` pair
in this header is structurally downstream of the pointer typed in
`ImageConstIteratorWithIndex::m_Position`. Implementing Unit 4 on
top of an un-migrated Unit 2 would require either (a) duplicating
the `VIsConst` machinery at two layers of the hierarchy, or (b)
keeping the `const_cast`s and papering over them — both are strictly
worse than sequencing.

Once Unit 2 lands:

- Unit 4 should be a <100-line diff.
- No behavioral change; only type-level plumbing.
- Validation: `git grep const_cast` on both headers empty after the
  change (modulo comment-only matches, of which there are currently
  none in either header).

## Validation snapshot (current tree, this branch)

```
$ git grep -n const_cast Modules/Core/Common/include/itkImageRegionIteratorWithIndex.h \
                         Modules/Core/Common/include/itkImageRegionConstIteratorWithIndex.h
itkImageRegionIteratorWithIndex.h:111:    ... const_cast<InternalPixelType *>(this->m_Position) ...
itkImageRegionIteratorWithIndex.h:120:    return *(const_cast<InternalPixelType *>(this->m_Position));
```

The const header is already clean. Post-Unit-2 + Unit-4, both
should be comment/match-free.
