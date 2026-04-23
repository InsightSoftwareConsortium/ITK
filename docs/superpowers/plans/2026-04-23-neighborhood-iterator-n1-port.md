# NeighborhoodIterator N+1 Port Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port method bodies from the four legacy neighborhood-iterator classes into the templated `NeighborhoodIteratorBase` / `ShapedNeighborhoodIteratorBase` in `itkNeighborhoodIteratorBase.h`, then replace the legacy headers with alias declarations. Eliminates all 4 `const_cast` sites in the neighborhood-iterator family.

**Architecture:**
The spike at `Modules/Core/Common/include/itkNeighborhoodIteratorBase.h` defines two class templates parameterized on `bool VIsConst`, using `std::conditional_t` to select pointer types. This plan fills in the method bodies (currently stubs / missing) by porting from `itkConstNeighborhoodIterator.{h,hxx}`, `itkNeighborhoodIterator.h`, `itkConstShapedNeighborhoodIterator.{h,hxx}`, and `itkShapedNeighborhoodIterator.h`. Each legacy header then becomes a thin alias file. Write methods (`SetPixel`, `SetCenterPixel`) become SFINAE-disabled on the const instantiation.

**Tech Stack:** C++17 (std::conditional_t, if constexpr, std::enable_if_t), ITK CMake/Ninja build, ITKCommon2TestDriver.

**Scope:** This is a multi-PR effort. Each milestone below is a reviewable PR that builds green and passes tests. Do NOT attempt to land all milestones in a single PR.

---

## File Structure

### Files modified across all milestones

- `Modules/Core/Common/include/itkNeighborhoodIteratorBase.h` — grow from spike to full implementation. Expected end-state ~1100 LOC (was 541 as spike).
- `Modules/Core/Common/include/itkConstNeighborhoodIterator.h` — reduce to alias forward for `NeighborhoodIteratorBase<T,BC,true>`.
- `Modules/Core/Common/include/itkConstNeighborhoodIterator.hxx` — delete (bodies moved inline into base header) OR keep as explicit-instantiation shim during transition.
- `Modules/Core/Common/include/itkNeighborhoodIterator.h` — alias for `NeighborhoodIteratorBase<T,BC,false>`.
- `Modules/Core/Common/include/itkConstShapedNeighborhoodIterator.h` — alias for `ShapedNeighborhoodIteratorBase<T,BC,true>`.
- `Modules/Core/Common/include/itkConstShapedNeighborhoodIterator.hxx` — delete or shim.
- `Modules/Core/Common/include/itkShapedNeighborhoodIterator.h` — alias for `ShapedNeighborhoodIteratorBase<T,BC,false>`.
- `Modules/Core/Common/test/itkNeighborhoodIteratorBaseSpikeTest.cxx` — expand from compile-only to runtime test.
- `Modules/Core/Common/test/CMakeLists.txt` — add new runtime tests.

### No file split needed

The four legacy classes collapse into one header file (plus the shaped composition). This is the point of the refactor.

---

## Milestone 1 — Port ConstNeighborhoodIterator bodies (PR #1)

**Scope:** Port the ~35 methods of `ConstNeighborhoodIterator` from `itkConstNeighborhoodIterator.hxx` into `NeighborhoodIteratorBase<T,BC,true>` in the base header. Do NOT touch legacy headers yet; the base template is exercised only via the spike test and new runtime tests. `const_cast<ImageType*>` at `itkConstNeighborhoodIterator.hxx:648` is NOT present in the ported code (the member `m_Image` in the base is already `const ImageType*` via `std::conditional_t`).

**Files:**
- Modify: `Modules/Core/Common/include/itkNeighborhoodIteratorBase.h` — add method bodies
- Create: `Modules/Core/Common/test/itkNeighborhoodIteratorBaseRuntimeTest.cxx`
- Modify: `Modules/Core/Common/test/CMakeLists.txt`

### Task 1.1: Audit method surface

- [ ] **Step 1:** Read `Modules/Core/Common/include/itkConstNeighborhoodIterator.h` end-to-end. List every public / protected method signature in a scratch file at `docs/superpowers/plans/n1-method-audit.md`. Group by: read-only (port to base, both instantiations), write (port to base, SFINAE-disable on `VIsConst=true`), constructor, operator.
- [ ] **Step 2:** Cross-reference against `itkConstNeighborhoodIterator.hxx` — list line ranges for each out-of-line body.
- [ ] **Step 3:** Commit audit doc only.

```bash
git add docs/superpowers/plans/n1-method-audit.md
git commit -m "DOC: Audit ConstNeighborhoodIterator method surface for N+1 port"
```

### Task 1.2: Port constructors and member initializers

- [ ] **Step 1:** In `itkNeighborhoodIteratorBase.h`, implement the default ctor, the `(radius, ptr, region)` ctor, and the copy ctor of `NeighborhoodIteratorBase`. Source: `itkConstNeighborhoodIterator.hxx:206-246` (copy ctor), and corresponding header `itkConstNeighborhoodIterator.h` constructor definitions. The non-const → const converting ctor already exists in the spike — verify it still compiles.
- [ ] **Step 2:** Build `ITKCommon` only: `ninja -C build ITKCommon`. Expected: PASS.
- [ ] **Step 3:** Commit.

```bash
git add Modules/Core/Common/include/itkNeighborhoodIteratorBase.h
git commit -m "ENH: Port NeighborhoodIterator constructors into templated base"
```

### Task 1.3: Port read-only query methods

- [ ] **Step 1:** Port `InBounds()`, `IndexInBounds(n, internalIndex, offset)`, `IndexInBounds(n)`, `GetPixel(n, IsInBounds)`, `GetPixel(n)`, `GetCenterPixel()`, `ComputeInternalIndex(n)`, `GetBoundingBoxAsImageRegion()`, `GetNeighborhood()`, `GetIndex()`, `GetRegion()`, `GetRadius()`, `GetSize()`, `Size()`, `GetImagePointer()`. Copy bodies verbatim from `itkConstNeighborhoodIterator.hxx` substituting `NeighborhoodIteratorBase<TImage, TBC, VIsConst>` for `ConstNeighborhoodIterator<TImage, TBC>`.
- [ ] **Step 2:** Build: `ninja -C build ITKCommon`. Expected: PASS.
- [ ] **Step 3:** Commit.

```bash
git commit -am "ENH: Port NeighborhoodIterator read-only queries into templated base"
```

### Task 1.4: Port iteration methods (GoToBegin, GoToEnd, operator++, operator--)

- [ ] **Step 1:** Port `GoToBegin()` (`itkConstNeighborhoodIterator.hxx:362-368`), `GoToEnd()` (:369-375), `operator++()` (:484-519), `operator--()` (:520-555), `IsAtEnd()`, `IsAtBegin()`, `SetLocation(index)`, `SetLoop(index)`, `SetRegion(region)` (:376-420), `SetBound(size)` (:619-...), `SetEndIndex()` (:247-263), `Initialize(radius, ptr, region)` (:421-436).
- [ ] **Step 2:** At the end of this task, verify `const_cast<ImageType*>(m_ConstImage.GetPointer())` from `:648` is NOT present in the ported `SetBound` or any other method — it should be unnecessary because `m_Image` is already `const ImageType*` on the const instantiation and `ImageType*` on the non-const instantiation.
- [ ] **Step 3:** Build: `ninja -C build ITKCommon`. Expected: PASS.
- [ ] **Step 4:** Commit.

```bash
git commit -am "ENH: Port NeighborhoodIterator iteration methods into templated base"
```

### Task 1.5: Port write methods with SFINAE guards

- [ ] **Step 1:** Port `SetPixel(n, v)`, `SetCenterPixel(v)`, `SetNeighborhood(n)`, and any other mutators from `NeighborhoodIterator` (non-const subclass in `itkNeighborhoodIterator.h`) into the base, each guarded by `template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>`.
- [ ] **Step 2:** Add `static_assert(!VIsConst, "SetPixel disabled on const iterator")` as the first line of each write method body, for better diagnostics when the SFINAE fires via a class-template-member call.
- [ ] **Step 3:** Build: `ninja -C build ITKCommon`. Expected: PASS.
- [ ] **Step 4:** Commit.

```bash
git commit -am "ENH: Port NeighborhoodIterator write methods with const SFINAE guards"
```

### Task 1.6: Port operator= and PrintSelf

- [ ] **Step 1:** Port `operator=(const Self&)` (`itkConstNeighborhoodIterator.hxx:437-483`) and `PrintSelf(os, indent)` (:556-618).
- [ ] **Step 2:** Build. Expected: PASS.
- [ ] **Step 3:** Commit.

```bash
git commit -am "ENH: Port NeighborhoodIterator operator= and PrintSelf"
```

### Task 1.7: Write runtime test against base template

- [ ] **Step 1:** Create `Modules/Core/Common/test/itkNeighborhoodIteratorBaseRuntimeTest.cxx`. Test plan:
  - Instantiate `NeighborhoodIteratorBase<Image<float,3>, ZeroFluxNeumannBoundaryCondition<...>, true>` (const) and `..., false>` (non-const) on a small 5×5×5 filled image.
  - Iterate both to end, count visited voxels, assert == region size.
  - Through the non-const, `SetCenterPixel(42.0f)` at one location; assert the image pixel is 42.
  - Converting ctor: construct a const iterator from the non-const one; iterate; assert same values.
- [ ] **Step 2:** Add to `Modules/Core/Common/test/CMakeLists.txt` under `ITKCommon2TestDriver`.
- [ ] **Step 3:** Build: `ninja -C build ITKCommon2TestDriver`. Expected: PASS.
- [ ] **Step 4:** Run: `ctest --test-dir build -R itkNeighborhoodIteratorBaseRuntimeTest -V`. Expected: PASS.
- [ ] **Step 5:** Commit.

```bash
git commit -am "ENH: Runtime test exercising NeighborhoodIteratorBase on both instantiations"
```

### Task 1.8: Open PR for Milestone 1

- [ ] **Step 1:** Push: `git push hj modernize-iterators-remove-const-cast`
- [ ] **Step 2:** `gh pr create --draft --base main` with body explaining:
  - Ports method bodies only. Legacy headers untouched.
  - Base template now functional; proven by runtime test.
  - No user-facing behavior change; no legacy `const_cast` removed yet.
  - Links to this plan.

---

## Milestone 2 — Port Shaped variants (PR #2)

**Scope:** Port `ConstShapedNeighborhoodIterator` and `ShapedNeighborhoodIterator` method bodies into `ShapedNeighborhoodIteratorBase` in the same header. The three `const_cast` sites at `itkConstShapedNeighborhoodIterator.h:216,256` and `itkShapedNeighborhoodIterator.h:223` are structurally eliminated because `ShapedNeighborhoodIteratorBase` composes (not inherits) `NeighborhoodIteratorBase<..., VIsConst>`, so no outer-pointer const-stripping is needed.

**Files:**
- Modify: `Modules/Core/Common/include/itkNeighborhoodIteratorBase.h`
- Extend: `Modules/Core/Common/test/itkNeighborhoodIteratorBaseRuntimeTest.cxx`

### Tasks

Same granularity as Milestone 1 (audit → port ctors → port ActivateIndex/DeactivateIndex/ClearActiveList → port the InnerIterator Begin/End and increment → port write methods with SFINAE → runtime test → PR).

Key invariants to verify in review:
- No `const_cast` anywhere in `ShapedNeighborhoodIteratorBase` or its `InnerIteratorT`.
- `InnerIteratorT<true>` cannot be constructed from `ShapedNeighborhoodIteratorBase<...,false>::end()` accidentally (compile-time check via static_assert in the test).

Deferred detail: write one task per method group (active-index management, inner iterator, write SFINAE). Estimate 6-8 tasks matching 1.2-1.8 structure.

---

## Milestone 3 — Wire legacy headers as aliases (PR #3)

**Scope:** Replace the six legacy headers with alias declarations. Bodies of `.hxx` files become empty (retained only to satisfy existing `#include`s in the tree until cleanup in Milestone 4). After this PR, the 4 `const_cast` sites listed in `git grep const_cast Modules/Core/Common/include/itk*NeighborhoodIterator*` are gone from the tree.

**Files:**
- Modify: all six legacy headers listed in File Structure above.

### Task 3.1: Replace itkConstNeighborhoodIterator.h with alias

- [ ] **Step 1:** Rewrite `itkConstNeighborhoodIterator.h` to:
  ```cpp
  #ifndef itkConstNeighborhoodIterator_h
  #define itkConstNeighborhoodIterator_h
  #include "itkNeighborhoodIteratorBase.h"
  namespace itk {
  template <typename TImage, typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>>
  using ConstNeighborhoodIterator = NeighborhoodIteratorBase<TImage, TBoundaryCondition, /*VIsConst=*/true>;
  }
  #endif
  ```
- [ ] **Step 2:** Empty out `itkConstNeighborhoodIterator.hxx` (keep the file; leave header guards only, plus a comment `// Intentionally empty: bodies moved to itkNeighborhoodIteratorBase.h`).
- [ ] **Step 3:** Full rebuild: `ninja -C build`. Expected: PASS (this is the risky step — any consumer that does `template<...> class ConstNeighborhoodIterator;` forward-decl breaks here; fix call sites in separate tasks).
- [ ] **Step 4:** Run full test suite: `ctest --test-dir build -j8`. Expected: zero new failures vs. main baseline.
- [ ] **Step 5:** Commit.

```bash
git commit -am "ENH: Replace ConstNeighborhoodIterator header with alias to templated base"
```

### Task 3.2: Same for NeighborhoodIterator.h

(Analogous, `VIsConst=false`.)

### Task 3.3: Same for ConstShapedNeighborhoodIterator.{h,hxx}

(Analogous, uses `ShapedNeighborhoodIteratorBase`, `VIsConst=true`.)

### Task 3.4: Same for ShapedNeighborhoodIterator.h

(Analogous, `VIsConst=false`.)

### Task 3.5: Forward-decl fixups

- [ ] **Step 1:** `git grep "class ConstNeighborhoodIterator" Modules | grep -v "template"` — list every non-template forward decl.
- [ ] **Step 2:** Replace each with `#include "itkConstNeighborhoodIterator.h"` (alias templates cannot be forward-declared).
- [ ] **Step 3:** Repeat for the other three legacy names.
- [ ] **Step 4:** Full rebuild + test suite. Expected: PASS.
- [ ] **Step 5:** Commit.

```bash
git commit -am "COMP: Replace forward-decls of neighborhood iterators with includes"
```

### Task 3.6: Verify const_cast gone

- [ ] **Step 1:** `git grep -n const_cast Modules/Core/Common/include/itk*NeighborhoodIterator*`. Expected output: empty (the four legacy sites are gone; the new base header uses `std::conditional_t` instead).
- [ ] **Step 2:** PR description must include this git grep output as proof.

### Task 3.7: Open PR for Milestone 3

Flag explicitly for reviewer: forward-decl breakage is the only user-facing risk. Recommend running Slicer/BRAINSTools CI nightlies on this branch before merge.

---

## Milestone 4 — Cleanup (PR #4, after N+2 release cycle)

Per the rollout plan in `itkNeighborhoodIteratorBase.h`:
- Mark legacy class names `ITK_FUTURE_LEGACY_REMOVE`.
- Delete empty `.hxx` shims.
- Rename `NeighborhoodIteratorBase` → `NeighborhoodIterator` + update aliases.

**Out of scope for current effort** (belongs to a later release).

---

## Self-Review Checklist (to run before executing)

- [ ] Every `const_cast` site listed in initial grep (4 sites) has a task that removes it: `itkConstNeighborhoodIterator.hxx:648` (Task 1.4 verification), `itkConstShapedNeighborhoodIterator.h:216,256` + `itkShapedNeighborhoodIterator.h:223` (Milestone 2 verification + Milestone 3 alias swap).
- [ ] Each milestone produces a building, testing, mergeable PR.
- [ ] Converting ctor (non-const → const) tested at runtime (Task 1.7).
- [ ] Compile-fail cases (const → non-const ctor, SetPixel on const) already tested in existing `itkNeighborhoodIteratorBaseSpikeTest.cxx` static_asserts.
- [ ] Downstream forward-decl risk called out (Task 3.5, Milestone 3 PR body).

## Execution Note

Each milestone is substantial (~6-8 tasks, ~500-1000 LOC of mechanical porting + verification). Recommend **Subagent-Driven execution** with one subagent per task and reviewer checkpoint between milestones. Do not attempt all four milestones in one session.
