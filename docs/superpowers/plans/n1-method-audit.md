# ConstNeighborhoodIterator Method Audit (N+1 Port)

Inventory of every public / protected member and free function in
`Modules/Core/Common/include/itkConstNeighborhoodIterator.h` (header line
numbers) cross-referenced against out-of-line bodies in
`Modules/Core/Common/include/itkConstNeighborhoodIterator.hxx`.

This document supports the "NeighborhoodIterator N+1 Port" plan
(`2026-04-23-neighborhood-iterator-n1-port.md`): the four legacy classes
(`ConstNeighborhoodIterator`, `NeighborhoodIterator`,
`ConstShapedNeighborhoodIterator`, `ShapedNeighborhoodIterator`) will be
ported into two class templates parameterized on `bool VIsConst` in
`itkNeighborhoodIteratorBase.h`.

## Porting categories

- **ctor** — constructor / destructor / assignment. Ported to base; both
  instantiations use them.
- **read** — read-only accessor. Ported to base; both instantiations.
- **write** — mutates image data or sets boundary condition. Needs
  SFINAE-disabling on `VIsConst == true`.
- **iter** — iteration / traversal state mutator (`operator++`,
  `GoToBegin`, etc.). Mutates iterator state only, not image data — so
  available on both const and non-const instantiations (**no SFINAE
  needed**).
- **op** — overloaded operator (comparison / arithmetic on iterator).
- **helper** — protected helper used internally.
- **free** — non-member template free function at namespace scope.

## Legend

- `.h` — line in `itkConstNeighborhoodIterator.h`.
- `.hxx` — line range of out-of-line definition in
  `itkConstNeighborhoodIterator.hxx`, or `inline` if the body is in the
  header.
- `SFINAE?` — will the N+1 port need to disable this method when
  `VIsConst == true`? (Only methods that write pixel data need this;
  `OverrideBoundaryCondition` / `ResetBoundaryCondition` /
  `SetBoundaryCondition` already exist on the Const class, so they
  stay available on both instantiations.)

## Public methods

| Signature | .h | .hxx | Category | SFINAE? |
|---|---|---|---|---|
| `ConstNeighborhoodIterator() = default` | 101 | inline (defaulted) | ctor | no |
| `~ConstNeighborhoodIterator() override = default` | 104 | inline (defaulted) | ctor | no |
| `ConstNeighborhoodIterator(const ConstNeighborhoodIterator &)` | 107 | 206–245 | ctor | no |
| `ConstNeighborhoodIterator(const SizeType &, const ImageType *, const RegionType &)` | 111–121 | inline | ctor | no |
| `Self & operator=(const Self & orig)` | 124–125 | 437–482 | ctor / op | no |
| `void PrintSelf(std::ostream &, Indent) const override` | 128–129 | 556–617 | read | no |
| `OffsetType ComputeInternalIndex(const NeighborIndexType n) const` | 133–134 | 181–194 | read | no |
| `IndexType GetBound() const` | 137–141 | inline | read | no |
| `IndexValueType GetBound(NeighborIndexType n) const` | 145–149 | inline | read | no |
| `const InternalPixelType * GetCenterPointer() const` | 152–156 | inline | read | no |
| `PixelType GetCenterPixel() const` | 160–164 | inline | read | no |
| `const ImageType * GetImagePointer() const` | 167–171 | inline | read | no |
| `IndexType GetIndex() const` | 175–179 | inline | read | no |
| `IndexType GetFastIndexPlusOffset(const OffsetType & o) const` | 181–185 | inline | read | no |
| `NeighborhoodType GetNeighborhood() const` | 189–190 | 264–360 | read | no |
| `PixelType GetPixel(const NeighborIndexType i) const` | 193–207 | inline | read | no |
| `PixelType GetPixel(NeighborIndexType n, bool & IsInBounds) const` | 214–215 | 145–179 | read | no |
| `PixelType GetPixel(const OffsetType & o) const` | 219–225 | inline | read | no |
| `PixelType GetPixel(const OffsetType & o, bool & IsInBounds) const` | 232–236 | inline | read | no |
| `PixelType GetNext(const unsigned int axis, NeighborIndexType i) const` | 241–245 | inline | read | no |
| `PixelType GetNext(const unsigned int axis) const` | 250–254 | inline | read | no |
| `PixelType GetPrevious(const unsigned int axis, NeighborIndexType i) const` | 259–263 | inline | read | no |
| `PixelType GetPrevious(const unsigned int axis) const` | 268–272 | inline | read | no |
| `IndexType GetIndex(const OffsetType & o) const` | 276–280 | inline | read | no |
| `IndexType GetIndex(NeighborIndexType i) const` | 284–288 | inline | read | no |
| `RegionType GetRegion() const` | 291–295 | inline | read | no |
| `IndexType GetBeginIndex() const` | 299–303 | inline | read | no |
| `RegionType GetBoundingBoxAsImageRegion() const` | 307–308 | 196–204 | read | no |
| `OffsetType GetWrapOffset() const` | 311–315 | inline | read | no |
| `OffsetValueType GetWrapOffset(NeighborIndexType n) const` | 322–326 | inline | read | no |
| `void GoToBegin()` | 329–330 | 362–367 | iter | no |
| `void GoToEnd()` | 334–335 | 369–374 | iter | no |
| `void Initialize(const SizeType &, const ImageType *, const RegionType &)` | 339–340 | 421–435 | iter | no |
| `bool IsAtBegin() const` | 344–348 | inline | read | no |
| `bool IsAtEnd() const` | 352–366 | inline | read | no |
| `Self & operator++()` | 372–373 | 484–518 | iter / op | no |
| `Self & operator--()` | 379–380 | 520–554 | iter / op | no |
| `bool operator==(const Self & it) const` | 385–389 | inline | op | no |
| `ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self)` | 391 | macro-expanded | op | no |
| `bool operator<(const Self & it) const` | 396–400 | inline | op | no |
| `bool operator<=(const Self & it) const` | 405–409 | inline | op | no |
| `bool operator>(const Self & it) const` | 414–418 | inline | op | no |
| `bool operator>=(const Self & it) const` | 423–427 | inline | op | no |
| `void SetLocation(const IndexType & position)` | 433–438 | inline | iter | no |
| `Self & operator+=(const OffsetType &)` | 443–444 | 688–723 | iter / op | no |
| `Self & operator-=(const OffsetType &)` | 449–450 | 725–759 | iter / op | no |
| `OffsetType operator-(const Self & b) const` | 453–457 | inline | op | no |
| `bool InBounds() const` | 462–463 | 22–46 | read | no |
| `bool IndexInBounds(const NeighborIndexType, OffsetType &, OffsetType &) const` | 476–477 | 48–101 | read | no |
| `bool IndexInBounds(const NeighborIndexType n) const` | 481–482 | 103–143 | read | no |
| `void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)` | 489–493 | inline | write (BC ptr) | no |
| `void ResetBoundaryCondition()` | 497–501 | inline | write (BC ptr) | no |
| `void SetBoundaryCondition(const TBoundaryCondition & c)` | 504–508 | inline | write (BC) | no |
| `ImageBoundaryConditionPointerType GetBoundaryCondition() const` | 511–515 | inline | read | no |
| `void NeedToUseBoundaryConditionOn()` | 518–522 | inline | write (flag) | no |
| `void NeedToUseBoundaryConditionOff()` | 524–528 | inline | write (flag) | no |
| `void SetNeedToUseBoundaryCondition(bool b)` | 530–534 | inline | write (flag) | no |
| `bool GetNeedToUseBoundaryCondition() const` | 536–540 | inline | read | no |
| `void SetRegion(const RegionType & region)` | 543–544 | 376–419 | iter | no |

## Protected methods

| Signature | .h | .hxx | Category | SFINAE? |
|---|---|---|---|---|
| `void SetLoop(const IndexType & p)` | 549–554 | inline | helper | no |
| `void SetBound(const SizeType &)` | 559–560 | 619–641 | helper | no |
| `void SetPixelPointers(const IndexType &)` | 566–567 | 643–686 | helper | no |
| `void SetBeginIndex(const IndexType & start)` | 571–575 | inline | helper | no |
| `void SetEndIndex()` | 579–580 | 247–262 | helper | no |

## Non-member free functions (namespace itk)

| Signature | .h | .hxx | Category | SFINAE? |
|---|---|---|---|---|
| `operator+(const ConstNeighborhoodIterator<TImage> &, const OffsetType &)` | 649–657 | inline | free / op | no |
| `operator+(const OffsetType &, const ConstNeighborhoodIterator<TImage> &)` | 659–665 | inline | free / op | no |
| `operator-(const ConstNeighborhoodIterator<TImage> &, const OffsetType &)` | 667–675 | inline | free / op | no |

Note: these free functions take a single-parameter
`ConstNeighborhoodIterator<TImage>` but the class template itself has
two parameters (`TImage, TBoundaryCondition`). In the N+1 port these
free operators will need to be updated to carry the full template
parameter list (or be templated on the iterator type).

## Methods that *do* need SFINAE disabling in the N+1 port

None on `ConstNeighborhoodIterator` itself — by definition this is the
read-only class. The SFINAE-on-`VIsConst` requirement applies to the
*write* methods contributed by the non-const `NeighborhoodIterator`
subclass (`SetPixel`, `SetCenterPixel`, `SetNeighborhood`, `SetNext`,
`SetPrevious`, image-mutating `operator*`, etc.). Those will be
audited separately when porting `itkNeighborhoodIterator.h`.

The boundary-condition mutators (`OverrideBoundaryCondition`,
`ResetBoundaryCondition`, `SetBoundaryCondition`,
`SetNeedToUseBoundaryCondition` and the `…On()` / `…Off()` helpers)
mutate iterator state, not pixel data, and therefore remain available
on both const and non-const instantiations of the N+1 base.

## Observations / surprises

- `ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self)` (line 391) expands to
  `operator!=` via macro; the port must expand it the same way (or
  invoke the same macro) so the unequal operator is generated.
- The free-function `operator+` / `operator-` overloads (lines 649–675)
  are templated on `TImage` only — they do not carry the
  `TBoundaryCondition` parameter. Porting to the N+1 base will require
  re-templating them on the full base-class template arguments.
- The copy constructor at lines 107 / 206–245 *and* `operator=` at
  lines 124 / 437–482 both manually copy every member; they are not
  trivially defaulted. This is because `m_BoundaryCondition` is a raw
  pointer that may alias `&m_InternalBoundaryCondition` and must be
  re-bound after copy.
- `m_InBounds[Dimension]` (line 626) uses a plain C array sized by the
  compile-time `Dimension` constant; survives as-is in the port.
- Several trivial public getters (`GetBound`, `GetIndex`, `GetRegion`,
  `GetWrapOffset`) have both zero-arg and one-arg overloads — the port
  must preserve both.
- `IsAtEnd()` (lines 352–366) throws an `ExceptionObject` if the center
  pointer is past the end, which is an unusual side effect for a
  const-qualified predicate. Preserve verbatim.
