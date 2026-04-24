# SMOKE Unit 16 — ShapedFloodFilledImageFunctionConditionalIterator pair

Reconnaissance-only notes for the iterator `const_cast` removal spike
(branch `modernize-iterators-remove-const-cast`, base `smoke-base`).
No production code changes in this commit.

## Target files

- `Modules/Core/Common/include/itkShapedFloodFilledImageFunctionConditionalIterator.h`
  (mutable subclass — carries the two `const_cast` sites)
- `Modules/Core/Common/include/itkShapedFloodFilledImageFunctionConditionalConstIterator.h`
  (`const` superclass — no `const_cast`, already correct)

## `const_cast` sites in the mutable subclass

Both sites in `itkShapedFloodFilledImageFunctionConditionalIterator.h`:

### Site A — line 103 (`Get()`)

```cpp
const PixelType
Get() const override
{
  return const_cast<ImageType *>(this->m_Image.GetPointer())->GetPixel(this->m_IndexStack.front());
}
```

**Classification: Category-3 quick win.** `ImageBase::GetPixel(IndexType)` has
a `const` overload that returns `const PixelType &`. Removing the
`const_cast` and calling `this->m_Image->GetPixel(...)` directly will
compile — `m_Image` is `typename ImageType::ConstWeakPointer` (declared in
`itkConditionalConstIterator.h:109`), which yields a
`const ImageType *` when dereferenced, which in turn selects the const
`GetPixel` overload returning `const PixelType &`. The current return
type `const PixelType` (by-value const) is implicitly constructible from
that reference, so the signature is preserved. This fix can be applied
in isolation with zero ripple.

### Site B — line 110 (`Set()`)

```cpp
void
Set(const PixelType & value)
{
  const_cast<ImageType *>(this->m_Image.GetPointer())->GetPixel(this->m_IndexStack.front()) = value;
}
```

**Classification: architectural — requires VIsConst-parameterized base.**
The non-const `GetPixel` overload returns `PixelType &` (assignable), but
the subclass only has a `ConstWeakPointer m_Image` inherited from
`ConditionalConstIterator`. The `const_cast` is the only way to reach
the mutable overload given the current type hierarchy. Replacing it
requires either:

1. Templating `ConditionalConstIterator` (and the entire
   `FloodFilledFunctionConditionalConstIterator` →
   `ShapedFloodFilledFunctionConditionalConstIterator` →
   `ShapedFloodFilledImageFunctionConditionalConstIterator` chain) on a
   `bool VIsConst` parameter, storing
   `conditional_t<VIsConst, ConstWeakPointer, WeakPointer> m_Image`, and
   SFINAE-gating `Set` on `!VIsConst`; legacy alias templates
   `using ShapedFloodFilledImageFunctionConditionalIterator = Base<..., /*VIsConst=*/false>;`
   and `...ConstIterator = Base<..., /*VIsConst=*/true>;`. Ripple: every
   sibling iterator that inherits the same base (flood-filled family,
   shaped-flood-filled family, many others) also changes, because
   `ConditionalConstIterator` is a very wide base. This is a prerequisite
   landing — not appropriate for a single smoke unit.

2. Narrower alternative: introduce a sibling
   `ShapedFloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, bool VIsConst>`
   that does *not* derive from the existing `Const` class, and re-express
   both the const and non-const public names as alias templates of that
   base. This avoids touching `ConditionalConstIterator` but duplicates
   the flood-fill traversal state (`m_IndexStack`, `m_Seeds`, etc.) and
   therefore doubles maintenance. Also not appropriate as a single
   smoke unit without first deciding between option 1 and 2 globally.

## Referenced pattern

The spike plan cites `Modules/Core/Common/include/itkNeighborhoodIteratorBase.h`
as the canonical pattern for a VIsConst-templated base with legacy
alias templates. That file does **not** exist in the current tree
(`ls Modules/Core/Common/include/itkNeighborhoodIteratorBase.h`: no
such file). The closest analogues actually present are the prior
smoke units that followed the `*Base<..., VIsConst>` + alias-template
recipe on simpler image-region iterators (Units 2-12). Unit 16's
underlying base (`ConditionalConstIterator`) is structurally different
from `ImageConstIteratorWithIndex` — it uses `ConstWeakPointer m_Image`
rather than a raw `const InternalPixelType *` position pointer, so the
existing smoke-unit-2/3/4 fix cannot be transplanted verbatim.

## Ripple / consumers

`git grep` finds the pair referenced in:

- `Modules/Core/Common/include/itkShapedFloodFilledImageFunctionConditionalConstIterator.{h,hxx}` (self)
- `Modules/Segmentation/RegionGrowing/include/itkConnectedThresholdImageFilter.hxx`
- `Modules/Nonunit/Review/test/itkShapedFloodFilledImageFunctionConditionalConstIteratorTest{1,2,3}.cxx`

All five external references use the class names as typenames only; if
alias templates preserve the legacy names, no consumer edit is required.

## Recommendation

Split this unit into two follow-up PRs:

1. **Category-3 quick win** — remove the `const_cast` on line 103 and
   rely on the const `GetPixel` overload. Landable standalone, no
   ripple, compile-time proof only.
2. **VIsConst base refactor** — defer until `ConditionalConstIterator`
   (or a shared `FloodFilledFunctionConditionalIteratorBase`) has been
   templated on `VIsConst`. This is the prerequisite landing that Unit
   16's `Set()` site depends on; attempting it in isolation duplicates
   the flood-fill traversal state.

This smoke unit therefore produces no production code change — only
this findings document — and explicitly flags Unit 16 as **blocked on
a wider base-class refactor** that has not yet been scheduled in the
spike plan.
