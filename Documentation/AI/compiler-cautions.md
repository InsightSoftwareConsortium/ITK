# ITK Compiler Cautions and Refactoring Pitfalls

Distilled from 10+ years of COMP: commits, PR review comments, and CDash failure
analysis. Prioritizes C++11 to C++17 patterns.

## Supported Compiler Matrix

See [../docs/supported_compilers.md](../docs/supported_compilers.md) for the full compiler version matrix.

---

## 1. Smart Pointers to Forward-Declared Types

### 1a. In-Class `{}` Initializer on `unique_ptr<Incomplete>` — GCC 7–9.1

**Bug:** GCC 7–9.1 instantiates `~unique_ptr<T>()` when a member is brace-initialized
(`m_Foo{}`), even when `T` is only forward-declared at that point. This triggers
`error: invalid application of 'sizeof' to incomplete type`. Fixed in GCC 9.2.

```cpp
// BAD — GCC 7 compile error if GenericAdaptor is forward-declared:
std::unique_ptr<GenericAdaptor> m_Reader{};

// GOOD — default initialization is null anyway:
std::unique_ptr<GenericAdaptor> m_Reader;
```

This applies to any smart pointer to a forward-declared class, including
`itk::SmartPointer`. **References:** ITK PR #3877, PR #3645, PR #3927.

### 1b. `= default` Destructor in Header with `unique_ptr<Incomplete>`

This is a well-known C++ rule (Scott Meyers, *Effective Modern C++*, Item 22)
that surfaces frequently in ITK when adding `std::unique_ptr` members to
existing classes. The pattern to follow in ITK:

```cpp
// itkFooImageIO.h — DECLARE ONLY:
~FooImageIO() override;

// itkFooImageIO.cxx — DEFINE where the pointee type is complete:
FooImageIO::~FooImageIO() = default;
```

**References:** ITK PR #5997 (VoxBoCUB reader); N-Dekker review.

### 1c. Inline `= default` Destructor on Exported Concrete Classes — Shared Library ABI

The problem occurs when builds with `-fvisibility-inlines-hidden`.
This flag hides all inline function definitions from the shared
library export table, overriding the class-level visibility set by `ITK*_EXPORT`.
When `~Foo() override = default;` is written inline in the header, the
destructor thunks `D1Ev` and `D0Ev` become hidden symbols in the shared library
even though the vtable and typeinfo remain exported — silently breaking
`dynamic_cast` and runtime loading for pre-compiled consumers.

The pattern to follow for any non-template concrete class with `ITK*_EXPORT`:

```cpp
// itkFoo.h — DECLARE ONLY for exported concrete classes:
class ITKMODULE_EXPORT Foo : public Bar
{
public:
  ~Foo() override;   // declaration only — definition goes in .cxx
  // ...
};

// itkFoo.cxx:
// Out-of-line: ensures D1Ev/D0Ev are compiled with default (exported)
// visibility under -fvisibility-inlines-hidden.
// See: https://github.com/InsightSoftwareConsortium/ITK/issues/6000
Foo::~Foo() = default;
```

**Safe exceptions** (inline `= default` is acceptable):
- Abstract classes (never directly instantiated cross-DSO; vtable lives in the
  concrete subclass TU).
- Non-exported internal classes (no `ITK*_EXPORT` macro).
- Template classes (require explicit instantiation boilerplate to be defined in
  `.cxx`; a separate, more involved fix).

**References:** ITK issue #6000; ITK PR #6002 (fixes 30 concrete exported
non-template classes); ITK PR #5995 (original ABI report by N-Dekker).

---

## 2. Template Deduction and Dependent Type Issues

### 2a. `using typename Superclass::X` — MSVC 2017 (historical; VS2017 not supported)

> VS2017 is no longer in the support matrix but commits before 2022 contain many
> workarounds. Do **not** reintroduce this pattern even in new code.

```cpp
// Still avoid (clarity and portability):
using typename Superclass::CellType;   // ambiguous meaning

// Prefer (explicit and universally supported):
using CellType = typename Superclass::CellType;
```

### 2b. Missing `typename` for Dependent Types — AppleClang 12+

AppleClang 12 tightened enforcement of the C++17 rule requiring `typename` before
dependent type names. GCC is similarly strict in `-pedantic` mode.

```cpp
// BAD — AppleClang error:
static_cast<NumericTraits<SizeType>::PrintType>(val)

// GOOD:
static_cast<typename NumericTraits<SizeType>::PrintType>(val)
```

### 2c. Non-Type Template Parameter Type Mismatch in Specializations

When writing partial specializations that bridge two class templates with different
non-type parameter types (e.g., `itk::Matrix<T, unsigned int N>` vs
`cv::Matx<T, int N>`), the deduction variable must match *one* side exactly and
use `static_cast<>` for the other. C-style casts are not allowed per ITK style.

```cpp
// BAD — C-style cast, also MSVC/Clang warning:
template <typename T, int VRows, int VColumns>
struct Bridge<itk::Matrix<T, (unsigned int)VRows, (unsigned int)VColumns>,
              cv::Matx<T, VRows, VColumns>>;

// GOOD:
template <typename T, int VRows, int VColumns>
struct Bridge<itk::Matrix<T, static_cast<unsigned int>(VRows),
                             static_cast<unsigned int>(VColumns)>,
              cv::Matx<T, VRows, VColumns>>;
```

**References:** ITK PR #5994, N-Dekker review.

### 2d. `constexpr static` Member ODR-Use Inside Lambda — GCC 7, AppleClang

Pre-C++17: capturing or ODR-using a `constexpr static` data member inside a lambda
without an out-of-class definition causes a linker error ("undefined reference").

```cpp
// BAD — ODR-use of static constexpr in lambda body (pre-C++17 UB):
static constexpr unsigned int SupportSize = 4;
// ...
std::copy_n(src, SizeType{ SupportSize }, dst);  // linker error on GCC 7

// GOOD — copy to local:
const auto supportSize = SupportSize;
std::copy_n(src, SizeType{ supportSize }, dst);
```

In C++17 inline variables fix this for `inline constexpr`, but ITK targets GCC 7
which has partial C++17 support. **References:** Commit `27e4815b`.

---

## 3. `constexpr` Pitfalls

### 3a. `constexpr` Mixed with Runtime Values in `if constexpr`

All sub-expressions inside an `if constexpr` condition must be compile-time
constants.

```cpp
// BAD — compiles on 64-bit via short-circuit, but MSVC x86 error C2131:
if constexpr (sizeof(void *) < 8 && runtimeVar > 0xffffffff)

// GOOD — guard the runtime check inside the compile-time branch:
if constexpr (sizeof(void *) < 8)
{
  if (runtimeVar > 0xffffffff) { ... }
}
```

**References:** Commit `56748759`.

### 3b. `constexpr` Array of Function Pointers — MSVC

MSVC does not allow arrays of function pointers to be `constexpr`.

```cpp
// BAD — MSVC error:
constexpr void (*RegisterList[])(void) = { &RegisterFoo, &RegisterBar };

// GOOD:
void (* const RegisterList[])(void) = { &RegisterFoo, &RegisterBar };
```

**References:** Commit `3ad34a16`.

---

## 4. Undefined Behavior

### 4a. Bit Shift Overflow

Shifting by more bits than the type has width is UB. Use `std::min` to cap shift
amounts; use unsigned literals to avoid signed-overflow UB.

```cpp
// BAD — UB if sizeof(T) > sizeof(int):
return 1 << (8 * sizeof(T));

// GOOD:
constexpr size_t shift = std::min(8 * sizeof(T), 8 * sizeof(result_type) - 1);
return 1u << shift;
```

MSVC warns C4293 ("shift count negative or too big").

### 4b. Implicit Integer Truncation (UBSan)

Assigning a value outside the target type's representable range is UB. Caught by
`-fsanitize=integer` (UBSan). Common in test code setting pixel values.

```cpp
// BAD — UB if PixelType = uint8_t and value is 256:
PixelType pixel = 256;

// GOOD:
auto pixel = static_cast<PixelType>(256 % (std::numeric_limits<PixelType>::max() + 1));
```

### 4c. `operator[]` / `data()` on Empty `std::vector`

Converting `new T[N]` to `std::vector<T>` is safe, but `&vec[0]` and
`vec.data()` when `vec` is empty are UB (caught by UBSan). The old `new T[0]`
returned a valid non-null pointer; `std::vector` does not guarantee that.

```cpp
// BAD — UB when empty:
cell->SetPointIds(&pointIds[0]);

// GOOD:
if (!pointIds.empty())
{
  cell->SetPointIds(pointIds.data());
}
```

**References:** PR #5967.

### 4d. Uninitialized Variables in Template Loop Bodies

GCC reports UB via `-Waggressive-loop-optimizations` when a variable used inside a
loop body might be unread on the first iteration. Always initialize local variables
at declaration:

```cpp
// BAD:
double result;
for (unsigned int i = 0; i < N; ++i) result += arr[i];

// GOOD:
double result = 0.0;
for (unsigned int i = 0; i < N; ++i) result += arr[i];
```

---

## 5. Platform and Architecture Specifics

### 5a. `char` Signedness — ARM Linux

On ARM Linux, `char` defaults to **unsigned**. Code that assumes `char` is signed
(e.g., comparing `char` against negative values, using `char` for 8-bit signed
pixel types) fails silently or wraps incorrectly.

```cpp
// BAD — breaks on ARM where char is unsigned:
using ComponentType = char;
if (value < 0) ...    // always false on ARM

// GOOD:
using ComponentType = signed char;   // when signed semantics required
using ComponentType = unsigned char; // when unsigned semantics required
```

**References:** PR #5463, PR #5137.

### 5b. Missing `<cstdint>` — GCC 13+

GCC 13+ made system headers self-contained; `uint8_t`, `int32_t`, etc. are no
longer pulled in transitively. Always include `<cstdint>` explicitly.

```cpp
// In any file using uint8_t, int64_t, etc.:
#include <cstdint>
```

**References:** Commits `9c87ac96`, `4f275769`.

### 5c. Reserved Identifiers — AppleClang (`-Wreserved-identifier`)

Any identifier containing `__` (double underscore) anywhere, or starting with `_`
followed by a capital letter, or starting with `_` in global scope, is reserved
by the C++ standard. AppleClang enforces this strictly.

```cpp
// BAD:
double d__1;    // contains __
int __n;        // starts with __
int _Quality;   // _ + capital letter

// GOOD:
double d1;
int n;
int quality;
```

---

## 6. GCC-Specific Warnings

### 6a. `-Wmaybe-uninitialized` False Positives on ARM64 (GCC 11+)

GCC 11+ on aarch64 emits false-positive `-Wmaybe-uninitialized` warnings in
vectorized template code (Eigen3 SIMD kernels). Use platform-guarded suppression:

```cpp
#if defined(__GNUC__) && !defined(__clang__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
// ... affected vectorized/Eigen code ...
#if defined(__GNUC__) && !defined(__clang__)
#  pragma GCC diagnostic pop
#endif
```

The `!defined(__clang__)` guard is critical: Clang defines `__GNUC__` but does
not emit this false positive. **References:** Commit `da8e99c5`, PR #5953.

### 6b. `-Wdeprecated-copy` — GCC 11+ (Rule of Zero)

Declaring *any* special member function (even `= default` destructor) without
declaring all six triggers `-Wdeprecated-copy` in GCC 11+.

```cpp
// BAD — declares destructor but not copy constructor:
class Foo {
  ~Foo() = default;
  Foo& operator=(const Foo&) = default;
};

// GOOD — Rule of Zero (declare none if no custom resource management):
class Foo {};

// GOOD — Rule of Five (declare all if any are custom):
class Foo {
  Foo(const Foo&) = default;
  Foo(Foo&&) noexcept = default;
  Foo& operator=(const Foo&) = default;
  Foo& operator=(Foo&&) noexcept = default;
  ~Foo() = default;  // in .cxx if unique_ptr<Incomplete> members
};
```

---

## 7. MSVC-Specific Warnings

### 7a. C4805 — Unsafe Mix of `bool` and `int` with `|=`

MSVC warns when `|=` is used between a `bool` variable and an `int` expression.
Common pattern: accumulating `EXIT_SUCCESS`/`EXIT_FAILURE` into a `bool`.

```cpp
// BAD — MSVC C4805:
bool testStatus = EXIT_SUCCESS;
testStatus |= runTest();

// GOOD:
int testStatus = EXIT_SUCCESS;
testStatus |= runTest();
```

**References:** Commit `c9dcddfb`.

---

## 8. Clang / AppleClang Warnings

### 8a. `-Wzero-as-null-pointer-constant` in Third-Party Headers

Third-party C headers (HDF5, TIFF, JPEG, etc.) use `NULL` in C++ contexts.
Wrap their includes with ITK's suppression macros from `itkMacro.h`:

```cpp
#include "itkMacro.h"
ITK_CLANG_PRAGMA_PUSH
ITK_CLANG_SUPPRESS_Wzero_as_null_pointer_constant
#include "third_party_header.h"
ITK_CLANG_PRAGMA_POP
```

**References:** Commit `6463793d`, PR #5980.

### 8b. `-Wduplicate-enum` for Third-Party Enum Values

```cpp
ITK_CLANG_PRAGMA_PUSH
ITK_CLANG_SUPPRESS_Wduplicate_enum
#include "third_party_with_dup_enums.h"
ITK_CLANG_PRAGMA_POP
```

### 8c. `-Wunused-lambda-capture`

Clang warns when a captured variable is not actually used by reference in the
lambda body. Fix: pass as argument or promote to `constexpr`.

```cpp
// BAD — captures n by ref but uses it by value only:
const auto result = [&r, &n]() { return r + n; }();

// GOOD — n by value, or pass as argument:
const auto result = [&r](int n) { return r + n; }(n);
```

### 8d. `-Wextra-semi`

Do not add an extra `;` after ITK macro invocations that already end with a
statement. ITK macros now use `ITK_MACROEND_NOOP_STATEMENT` which requires a
trailing `;` syntactically, so adding a second `;` triggers `-Wextra-semi`.

---

## 9. ITK API Deprecations That Cause Compile Errors

These become hard errors when `ITK_LEGACY_REMOVE=ON` (the CI default):

| Old | New |
|---|---|
| `itkTypeMacro(Self, Super)` | `itkOverrideGetNameOfClassMacro(Self)` |
| `ITK_DISALLOW_COPY_AND_ASSIGN(T)` | `ITK_DISALLOW_COPY_AND_MOVE(T)` |
| `itkStaticConstMacro(N, T, v)` | `static constexpr T N = v;` |
| `itkWarningMacro(msg)` outside `itk::Object` | Use `itk::OutputWindowDisplayWarningText()` directly |
| `itkTypeMacro` + manual `GetNameOfClass` | `itkOverrideGetNameOfClassMacro` only |

---

## 10. Shared Library / Symbol Visibility

### Missing `ITK_TEMPLATE_EXPORT` on Explicit Instantiations

On macOS and Linux shared-library builds with `-fvisibility=hidden`, explicit
template instantiations must be exported or they will not be found by consuming
libraries:

```cpp
// BAD — missing export; link failures in .dylib/.so builds:
template class MyFilter<float, 3>;

// GOOD:
template class ITK_TEMPLATE_EXPORT MyFilter<float, 3>;
```

**References:** Commits `5efe8f33`, `cafb5ff5`.

---

## 11. Python Binding Pitfalls

### 11a. `np.bool` removed (NumPy 1.24+)

```python
# BAD:
np.dtype(np.bool)

# GOOD:
np.dtype(np.bool_)
```

### 11b. `PySequence_Fast_GET_ITEM` removed (Python 3.14)

```cpp
// BAD — removed in Python 3.14:
item = PySequence_Fast_GET_ITEM(seq, i);

// GOOD — reference-counted correctly, with mandatory null guard:
item = PySequence_GetItem(seq, i);
if (item != nullptr)
{
  // ... use item ...
  Py_DECREF(item);
}
```

**References:** Commit `1726d3e5`, PR #5504.

---

## 12. clang-tidy Warnings During Refactoring

### 12a. Do Not Introduce New clang-tidy Diagnostics

Refactoring commits should leave the clang-tidy diagnostic count no worse than before.
If a refactoring triggers warnings in code it did not touch, those are pre-existing
issues and must **not** be fixed in the same commit. Mixing style fixes with
behavioral changes obscures commit intent and complicates `git bisect`.

### 12b. clang-tidy Check Families That Conflict with ITK Coding Standards

Several clang-tidy check categories produce warnings that are **incorrect or
inapplicable** in an ITK context. Do not "fix" these — doing so either breaks
ITK conventions or introduces irrelevant churn:

| Check family | Why it conflicts with ITK |
|---|---|
| `llvmlibc-*` | LLVM standard-library internals only; never applicable to ITK code |
| `readability-identifier-length` | ITK legitimately uses `i`, `j`, `k` for loop indices and `T` for template parameters |
| `readability-redundant-member-init` | ITK style prefers explicit base-class initialization in constructors |
| `modernize-use-trailing-return-type` | ITK uses conventional leading return types throughout |
| `google-*` | Enforces Google style, which differs from ITK naming and formatting rules |
| `cppcoreguidelines-avoid-magic-numbers` | ITK uses literal dimension constants (e.g., `2`, `3`) as template arguments |

### 12c. Suppressing False-Positive Checks

For legitimate ITK code that a clang-tidy check incorrectly flags, prefer a
`.clang-tidy` config exclusion (`Checks: '-llvmlibc-*'`) over inline
`// NOLINT` comments. Inline suppressions accumulate and can mask real issues.

---

## 13. Quick-Reference Checklist for Refactoring

When refactoring existing code, verify each item:

- [ ] `unique_ptr<T>` members: no `{}` init; destructor declared in `.h`, defined `= default` in `.cxx`
- [ ] All `new`/`delete` on forward-declared types are in `.cxx` files
- [ ] No C-style casts — use `static_cast<>`, `reinterpret_cast<>`, `const_cast<>`
- [ ] All `typename` keywords present before dependent type names
- [ ] `char` used for I/O — should be `signed char` or `unsigned char` explicitly
- [ ] `<cstdint>` included wherever `uint8_t`/`int64_t` etc. are used
- [ ] `std::vector` replacing `new[]` — guard `data()` / `operator[]` calls on potentially empty vectors
- [ ] Lambda captures — no unused captures; `constexpr` variables don't need capture
- [ ] Loop variables initialized at declaration
- [ ] `bool` not used with `|=` for exit-status accumulation — use `int`
- [ ] Third-party header includes wrapped with appropriate `ITK_CLANG_SUPPRESS_*` macros
- [ ] Explicit template instantiations in shared-build modules marked `ITK_TEMPLATE_EXPORT`
- [ ] ITK deprecated macros replaced (`itkTypeMacro`, `ITK_DISALLOW_COPY_AND_ASSIGN`, `itkStaticConstMacro`)
- [ ] No new clang-tidy diagnostics introduced; `llvmlibc-*`, `readability-identifier-length`, and `google-*` warnings ignored if pre-existing
