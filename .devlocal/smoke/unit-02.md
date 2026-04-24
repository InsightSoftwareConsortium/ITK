# Smoke Test Unit 02 — ImageIteratorWithIndex / ImageConstIteratorWithIndex

## Target

Eliminate `const_cast` at `Modules/Core/Common/include/itkImageIteratorWithIndex.h:112` and `:121`:

```cpp
void Set(const PixelType & value) const {
  this->m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(this->m_Position)), value);
}
PixelType & Value() {
  return *(const_cast<InternalPixelType *>(this->m_Position));
}
```

Pattern requested: `ImageIteratorWithIndexBase<TImage, bool VIsConst>` with pointer fields
`std::conditional_t<VIsConst, const X*, X*>`, `Set()`/`Value()` SFINAE-gated on `!VIsConst`,
legacy names become alias templates. Mirror `itkNeighborhoodIteratorBase.h`.

## Grep output before

```
$ git grep -n const_cast Modules/Core/Common/include/itkImageIteratorWithIndex.h \
                         Modules/Core/Common/include/itkImageConstIteratorWithIndex.h
Modules/Core/Common/include/itkImageIteratorWithIndex.h:112: this->m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(this->m_Position)), value);
Modules/Core/Common/include/itkImageIteratorWithIndex.h:121: return *(const_cast<InternalPixelType *>(this->m_Position));
```

## Grep output after

No change (see pitfalls — structural refactor deferred).

## Pattern-file status

`Modules/Core/Common/include/itkNeighborhoodIteratorBase.h` **does not exist** in this
worktree (base `ceedf987bd`, `origin/main`). The referenced canonical pattern has not yet
been merged. Spike proceeded by inspecting the task-specified structural intent only.

## Pitfalls identified

1. **`.hxx` out-of-line definitions block alias-template substitution.**
   Both `itkImageConstIteratorWithIndex.hxx` and `itkImageIteratorWithIndex.hxx` define
   member functions qualified as `ImageConstIteratorWithIndex<TImage>::...` and
   `ImageIteratorWithIndex<TImage>::...`. If the header turns those names into alias
   templates over a new `ImageIteratorWithIndexBase<TImage, VIsConst>`, the out-of-line
   definitions in the .hxx files would need to be re-qualified against the base template
   (and partial specializations would be needed since alias templates cannot be
   partially specialized on the member side). A real refactor must:
   - Move all out-of-line member bodies from both .hxx files into the new
     `ImageIteratorWithIndexBase<TImage, VIsConst>` template, specialized as needed.
   - Keep the .hxx includes for backwards-compatible include points, but have them
     forward to the base-template .hxx.

2. **`m_Image` storage type.**
   The Const iterator stores `typename TImage::ConstWeakPointer m_Image`. Under the
   templated-`VIsConst` base, the non-const form should store
   `typename TImage::WeakPointer m_Image` (or `std::conditional_t<VIsConst,
   ConstWeakPointer, WeakPointer>`). The Mutable constructor
   `ImageIteratorWithIndex(TImage * ptr, ...)` feeds into
   `ImageConstIteratorWithIndex(const TImage * ptr, ...)` — so at construction the
   non-constness is already lost. The base-template refactor is where the non-const
   pointer actually becomes storable; downstream iterators (ImageRegionIteratorWithIndex,
   ImageLinearIteratorWithIndex, etc.) that re-derive from these will need audit.

3. **Downstream ripple.**
   `git grep -l "public ImageConstIteratorWithIndex\|public ImageIteratorWithIndex"
   Modules/` returns the full With-Index iterator family:
   - `itkImageRegionIteratorWithIndex{,.h,.hxx}`
   - `itkImageRegionConstIteratorWithIndex{,.h,.hxx}`
   - `itkImageRegionExclusion(Const)?IteratorWithIndex`
   - `itkImageLinear(Const)?IteratorWithIndex`
   - `itkImageSlice(Const)?IteratorWithIndex`
   - `itkImageRandom(Const|NonRepeating)?IteratorWithIndex`
   - `itkImageReverse(Const)?Iterator`
   Each of these duplicates the const/mutable split. A clean VIsConst refactor at the
   base should propagate through all of them; otherwise, only the two base headers
   clean up while derived classes keep their own `const_cast`s.

4. **`wrapping/` references.**
   Python/wrapping layer rarely instantiates iterators directly; a quick grep in
   `wrapping/` shows no `ImageIteratorWithIndex` wrapping files. The structural
   refactor is unlikely to affect wrapping — but any ITK-external remote module
   that subclasses these iterators is at risk.

5. **Forward declarations.**
   No forward declarations of `ImageConstIteratorWithIndex` / `ImageIteratorWithIndex`
   appear in ITK core headers outside their own `.h` files. Alias-template conversion
   should be safe from a forward-decl perspective.

## Recommendation for real refactor

- **Do not land as a pure header refactor.** The .hxx files carry significant
  implementation and must move into the new `ImageIteratorWithIndexBase<TImage,
  VIsConst>` template in lockstep. Treat as a single ~4-file atomic change:
  `itkImageIteratorWithIndexBase.h` (new), `itkImageIteratorWithIndexBase.hxx`
  (new), `itkImageConstIteratorWithIndex.h` (alias), `itkImageIteratorWithIndex.h`
  (alias). Delete or shrink the existing .hxx files.
- **Sequence the derived-iterator family next.** A VIsConst-templated base is only
  a partial win if the ~8 derived With-Index iterator pairs keep their own const/
  mutable code duplication. Plan a follow-up unit per derived pair.
- **ABI check.** `ITK_TEMPLATE_EXPORT` on the class declarations plus the alias-
  template substitution may change mangled names of member functions. Verify
  against `nm -C` on a built `libITKCommon.*.dylib` before shipping.
- **Not suitable for a minimal spike commit.** This unit returns negative/structural
  findings only; the const_cast lines remain in place. A real PR should bundle the
  base-template extraction, the .hxx migration, and a quick ABI diff before
  review.

## Commit intent

This commit adds only this findings document. No source code changes — the
structural refactor is out of scope for a sub-10-minute reconnaissance pass.
