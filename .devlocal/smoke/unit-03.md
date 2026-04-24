# SMOKE Unit 03 — ImageRegionIterator / ImageRegionConstIterator

## Summary

Reconnaissance refactor that introduces

```cpp
template <typename TImage, bool VIsConst = true>
class ImageRegionIteratorBase : public ImageConstIterator<TImage> { ... };

template <typename TImage> using ImageRegionConstIterator = ImageRegionIteratorBase<TImage, true>;
template <typename TImage> using ImageRegionIterator      = ImageRegionIteratorBase<TImage, false>;
```

`Set()` and non-const `Value()` are SFINAE-gated on `!VIsConst` via a defaulted
`template <bool B = VIsConst, typename = std::enable_if_t<!B>>` parameter pack
so that only the writable specialization exposes them. A `conditional_t`-based
`BufferPointerType` / `ImagePointerType` alias is declared on the new base to
document the direction a real refactor must take.

## `git grep const_cast` on the two files

```
Modules/Core/Common/include/itkImageRegionConstIterator.h:145:   * const InternalPixelType*. Removing the last const_cast requires unit 1
Modules/Core/Common/include/itkImageRegionConstIterator.h:154:      *(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)), value); // SMOKE: parent-class ripple
Modules/Core/Common/include/itkImageRegionConstIterator.h:161:    return *(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)); // SMOKE: parent-class ripple
Modules/Core/Common/include/itkImageRegionIterator.h:33: * templated on const-ness in this worktree. The const_cast on m_Buffer therefore
```

Lines 145 / 33 are documentation. Lines 154 / 161 are the two surviving casts —
retained intentionally because the storage they operate on lives in the parent
class, not this unit (see Parent-class Ripple below).

## Parent-class ripple (the critical finding)

`ImageRegionConstIterator` inherits from `ImageConstIterator<TImage>` and
`ImageRegionIterator` inherits (in upstream) from `ImageIterator<TImage>`.
**Neither parent class is templated on `VIsConst`** in the current tree. Their
member pointers are fixed:

- `ImageConstIterator<TImage>::m_Buffer` is `const InternalPixelType *`
- `ImageConstIterator<TImage>::m_Image`  is `const TImage *`
- `ImageIterator<TImage>` derives from `ImageConstIterator<TImage>` and
  re-acquires a writable view via `const_cast` in its own `Set()` / `Value()`.

Consequence: within unit 03 alone, removing the last two `const_cast` calls is
impossible — the `m_Buffer` we dereference is typed `const InternalPixelType *`
by the parent. The SFINAE-gated writable API is structurally correct, but the
cast still has to appear until unit 1 (ImageIterator) is also templated on
`VIsConst`, at which point `m_Buffer` will be
`std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>`
and `Set()/Value()` can dereference without any cast.

A second, related ripple: `ImageRegionIterator(const ImageIterator<TImage>&)`
(the cast-from-mutable-parent constructor) has no counterpart in the const
case. When unit 1 lands, this constructor becomes
`ImageRegionIteratorBase(const ImageIteratorBase<TImage,false>&)` on the
`VIsConst==false` specialization only — another SFINAE gate.

## Wrapping / Python refs check

```
grep -rn 'ImageRegionIterator\|ImageRegionConstIterator' Wrapping/ Modules/Core/Common/wrapping/
# -> no matches
```

Neither iterator is wrapped for Python. The alias-template legacy names are
backward compatible at the C++ source level (both `ImageRegionIterator<ImgT>`
and `ImageRegionConstIterator<ImgT>` still name valid, inheritance-compatible
types). Downstream ABI: alias templates do NOT create new linker symbols, so
the existing instantiations across ITK remain valid.

## Pitfalls encountered / anticipated

1. **Protected constructors lost.** The upstream `ImageRegionIterator` kept the
   `const ImageRegionConstIterator&` constructor and assignment `protected` to
   enforce const-correctness. Collapsing into one templated base means there is
   no separate class on which to mark those members protected — the gate has
   to move to SFINAE on the conversion constructor or to a `static_assert`.
   The smoke prototype omits these to keep scope tight; a production refactor
   must restore them.
2. **`itkOverrideGetNameOfClassMacro(ImageRegionIteratorBase)`** now reports a
   single class name regardless of `VIsConst`. The legacy runtime names
   `ImageRegionIterator` / `ImageRegionConstIterator` are lost unless the
   macro is specialized per-`VIsConst` (trivial but not done in smoke).
3. **Deduction guide collision.** Upstream had a deduction guide for each of
   the two classes; collapsing into the base template means the const/non-const
   deduction must be derived from `std::is_const_v<TImage>` (shown in the file
   at the bottom of itkImageRegionConstIterator.h). Callers that wrote
   `ImageRegionIterator(imgPtr, region)` with CTAD will continue to work; callers
   that wrote `ImageRegionConstIterator(imgPtr, region)` get deduced via the
   alias template just fine.
4. **`.hxx` file (itkImageRegionConstIterator.hxx) defines `Increment()` /
   `Decrement()`** on `ImageRegionConstIterator<TImage>`. Those member
   definitions must be re-targeted to `ImageRegionIteratorBase<TImage,VIsConst>`
   with matching template parameters — not done here (header-only smoke).
5. **Parent ripple again:** the mutable `ImageRegionIterator(ImageIterator<T>&)`
   ctor could not be collapsed without conflicting with the const version on
   the const specialization. Handled via `enable_if` in a real refactor.

## Real-refactor recommendation

Order the hierarchy refactor bottom-up:

1. **Unit 1 first — template `ImageIteratorBase<TImage,VIsConst>`.** Until the
   parent is const-templated, every leaf iterator will continue to need a
   `const_cast` inside `Set()`/`Value()`. Unit 3 cannot fully eliminate its
   two casts in isolation.
2. Then propagate `VIsConst` into the region / linear / slice / reverse
   iterator families as alias-templated leaves (this unit is a template for
   that pattern).
3. Preserve protected-ctor semantics via SFINAE / `requires` clauses so that a
   writable iterator cannot be silently copy-constructed from a const one.
4. Preserve `itkOverrideGetNameOfClassMacro` output by specializing the base.
5. Move `.hxx` member definitions to the new base template.
6. Only then can `git grep const_cast Modules/Core/Common/include/itkImage*Iterator*.h`
   return empty.

Scope of the full refactor is modest (single Module, no Python wrapping) but
gated on unit 1 landing first.
