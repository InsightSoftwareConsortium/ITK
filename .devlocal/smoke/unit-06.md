# SMOKE Unit 6 — ImageLinearIteratorWithIndex{,Const}

Base: `0a8e45e164` (origin/smoke-base)
Branch: `smoke-unit-06-image-linear-iterator-with-index`

## Pattern

- Introduced `ImageLinearIteratorWithIndexBase<TImage, bool VIsConst>` in
  `itkImageLinearConstIteratorWithIndex.h`, inheriting
  `std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>`.
- `Set()` and non-const `Value()` are SFINAE-gated on `!VIsConst`.
- Both public names are now alias templates:
  - `ImageLinearConstIteratorWithIndex<TImage>  = Base<TImage, true>`
  - `ImageLinearIteratorWithIndex<TImage>       = Base<TImage, false>`
- Deduction guides re-provided for both CTAD entry points.
- Inline `NextLine` / `PreviousLine` definitions moved onto the base.
- Pattern mirrors `itkNeighborhoodIteratorBase.h` (unit reference).

## const_cast audit

```
$ git grep -n const_cast Modules/Core/Common/include/itkImageLinearIteratorWithIndex.h Modules/Core/Common/include/itkImageLinearConstIteratorWithIndex.h
...ImageLinearConstIteratorWithIndex.h:34: * ...eliminating the `const_cast<InternalPixelType *>`
...ImageLinearConstIteratorWithIndex.h:70:    // No const_cast: ...
...ImageLinearIteratorWithIndex.h:27: * ...specialization... The const_cast
```

All matches are comment-only. The two live `const_cast<InternalPixelType *>`
uses (formerly at lines 105 and 114 of `itkImageLinearIteratorWithIndex.h`)
are eliminated because the non-const base stores `m_Position` as
`InternalPixelType *`.

## Ripple — depends on unit 2 outcome

This change assumes `ImageIteratorWithIndex` (unit 2) exposes a protected
or derived-class-accessible `m_Position` / `m_PixelAccessorFunctor` matching
the `ImageConstIteratorWithIndex` layout. Any layout drift in the unit-2
rework will require matching adjustments here.

## Constructors dropped in the spike

The historical leaf-only constructors
(`ImageLinearIteratorWithIndex(const ImageIteratorWithIndex<TImage> & it)`
and protected `const ImageLinearConstIteratorWithIndex<TImage> &` helpers)
are intentionally omitted from the spike. A production port must reintroduce
them via SFINAE on `VIsConst` or via partial specialization; this is a
structural proof, not feature-complete.

## No build attempted (reconnaissance only).
