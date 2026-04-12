# ITK Code Review Lessons — Recurring Reviewer Concerns

Distilled from 8,484 inline review comments across 1,457 pull requests
spanning 2017–2026. These are patterns that ITK's core reviewers flag
repeatedly; an AI assistant that avoids them will produce PRs that pass
review with fewer round-trips.

---

## 1. Remove All Orphaned Artifacts After Refactoring

**Flagged on 31% of reviewed PRs (453/1,457). Active 2018–2026.**

When you remove the last user of a helper function, a local variable,
an `#include`, or a type alias — also remove the definition itself.
Reviewers catch orphaned code more than any other single issue.

```cpp
// BAD — removed the std::cout print that used chBuffer,
//        but left the clGetPlatformInfo() call that populated it.
clGetPlatformInfo(platform, CL_PLATFORM_NAME, sizeof(chBuffer), chBuffer, nullptr);

// GOOD — removed both the consumer AND the producer.
```

**Checklist before submitting a refactoring PR:**
- Grep for every symbol you modified or deleted.
- If a helper, typedef, or include has zero remaining callers, remove it.
- If a `using` alias was only public for one consumer, move it to `private`
  or remove it entirely.

---

## 2. Test Quality and GTest Conventions

**Flagged on 28% of reviewed PRs (411/1,457). Active 2017–2026.**

### Unique suite names per `.cxx` file

```cpp
// BAD — "HeavisideStepFunction" reused across multiple test files:
TEST(HeavisideStepFunction, ConvertedLegacyTest)  // in file A
TEST(HeavisideStepFunction, AnotherTest)          // in file B

// GOOD — one unique suite name per .cxx file:
TEST(SinRegularizedHeavisideStepFunction, ConvertedLegacyTest)
```

### Use `ConvertedLegacyTest` for migrated CTest tests

When converting a legacy `itkFooTest.cxx` to GTest, name the test
`TEST(Foo, ConvertedLegacyTest)` unless the test has a more specific
purpose worth naming.

### Non-fatal assertions need null guards

`ITK_TEST_EXPECT_TRUE` is non-fatal — it records failure but continues.
If a `dynamic_cast` might return null, guard before dereferencing:

```cpp
// BAD — continues past null and crashes:
auto * p = dynamic_cast<Derived *>(base.GetPointer());
ITK_TEST_EXPECT_TRUE(p != nullptr);
p->DoSomething();   // CRASH if dynamic_cast failed

// GOOD — bail immediately on null:
auto * p = dynamic_cast<Derived *>(base.GetPointer());
if (p == nullptr)
{
  std::cerr << "dynamic_cast failed" << std::endl;
  return EXIT_FAILURE;
}
```

---

## 3. Include and Header Hygiene

**Flagged on 21% of reviewed PRs (313/1,457). Active 2018–2026.**

- Include only what you use. Do not leave includes for removed code.
- Prefer forward declarations in headers when only a pointer or
  reference is needed.
- After removing code, check whether any `#include` directives became
  orphaned.

---

## 4. Naming Clarity

**Flagged on 17% of reviewed PRs (254/1,457). Active 2018–2026.**

- Variable names should describe what they hold, not how they were computed.
- After applying a limit or filter, rename the variable to reflect its
  new meaning (e.g., `nodes` → `displayed_nodes` after truncation).
- Magic numbers should be named constants or use ITK's existing named
  constants (e.g., `itk::Statistics::MersenneTwisterRandomVariateGenerator::DefaultSeed`
  instead of `121212`).

---

## 5. Style Consistency Within a Function

**Flagged on 15% of reviewed PRs (214/1,457). Active 2018–2026.**

After a partial fix, check that the modified code is consistent with the
rest of the function and file:

- If a function parameter type was updated, check that the function body
  uses the same qualified form.
- If a naming pattern was corrected, apply the correction to all similar
  instances in the same scope — do not leave a mix.
- Sub-section numbering, comment formatting, and JSON keys should be
  consistent within and across files.

---

## 6. Error Handling and Exception Safety

**Flagged on 14% of reviewed PRs (201/1,457). Active 2018–2026.**

- Check return values from functions that can fail (`dynamic_cast`,
  Python C API calls, file I/O). A `nullptr` or error return passed
  silently to the next line is a crash.
- After removing cleanup code (e.g., `delete m_Writer; m_Writer = nullptr;`),
  verify the replacement (`unique_ptr`, RAII) provides equivalent
  exception-safety guarantees.
- `EXPECT_NO_THROW` in GTest should wrap only the call being tested,
  not surrounding side-effects like `std::cout`.

---

## 7. Signed/Unsigned Conversions and `size_t`

**Flagged on 8% of reviewed PRs (120/1,457). Active 2018–2026.**

- Avoid narrowing conversions between signed and unsigned types.
  Prefer ITK's `SizeValueType` or `unsigned int` consistently.
- Do not add `static_cast` just to silence a warning — ask whether the
  conversion is actually safe. Unnecessary casts obscure real bugs.
- For template parameters, prefer `unsigned{VRows}` over
  `static_cast<unsigned int>(VRows)` — it is type-safe and rejects
  narrowing.

---

## 8. Locale-Safe Numeric Parsing

**Flagged on 8% of reviewed PRs (113/1,457). Active 2018–2026.**

`std::stod`, `std::stof`, `atof`, and `std::to_string` are
locale-dependent. Under European locales they produce/consume `,`
instead of `.` as the decimal separator, silently corrupting
medical image metadata.

```cpp
// BAD — locale-dependent:
double value = std::stod(str);
buffer << std::fixed << value;

// GOOD — locale-safe:
buffer << itk::ConvertNumberToString(value);
```

Use `itk::ConvertNumberToString()` for serialization.
See `itk-locale-safe-migration` for the full set of affected functions.

---

## 9. When to Use `auto`

**Flagged on 7% of reviewed PRs (105/1,457). Active 2018–2026.**

ITK is not anti-`auto`, but reviewers reject it when the deduced type is
not obvious from the initializer.

```cpp
// GOOD — type is obvious from the RHS:
const auto size = image->GetLargestPossibleRegion().GetSize();
auto filter = MedianImageFilter::New();
const auto it = container.begin();

// BAD — reader cannot guess the deduced type:
const auto value = interp->EvaluateDerivativeAtContinuousIndex(index);
// (value is a CovariantVector — not obvious)

// BETTER — spell out non-obvious types:
const CovariantVectorType value = interp->EvaluateDerivativeAtContinuousIndex(index);
```

**Rule of thumb:** use `auto` when the type name appears on the same line
(factory methods, iterators, casts) or is unambiguous from the method name
(`GetSize()`, `GetSpacing()`). Spell out the type when the return type
requires knowledge of the class's internal typedefs.

---

## 10. ITK Initializer Patterns

**Flagged on 4% of reviewed PRs (57/1,457). Active 2018–2026.**

Use the single-expression forms for FixedArray-based types:

```cpp
// BAD — two-line declare-then-fill:
SizeType size;
size.Fill(2);

// GOOD — single expression:
constexpr auto size = SizeType::Filled(2);

// BAD:
image->Allocate(true);

// GOOD:
image->AllocateInitialized();

// BAD — zero-initialize in two lines:
IndexType start;
start.Fill(0);

// GOOD — brace initialization is zero-fill:
constexpr IndexType start{};
```

---

## 11. No C-Style Casts; Prefer Local Fixes Over Cascading Changes

**Flagged on 10 PRs (0.7%). Active 2019–2026. Every instance was a correctness concern.**

```cpp
// BAD:
unsigned int rows = (unsigned int)VRows;

// GOOD — prefer no cast; if needed:
unsigned int rows = unsigned{VRows};    // type-safe, rejects narrowing
unsigned int rows = static_cast<unsigned int>(VRows);
```

Before adding any cast, ask: "Do I actually get a compiler warning without
this?" If not, the cast is unnecessary and obscures the code.

When a cast exists because a local variable has a mismatched type, it is
often better to change the local variable's type to eliminate the cast
entirely. However, **do not cascade type changes into function signatures,
template parameters, or public API boundaries** to avoid a cast. A small
local `static_cast` or `T{x}` conversion is preferable to changing an
API that downstream consumers depend on. The rule: fix the narrowest
scope that removes the cast without altering any interface.

---

## 12. Redundant Namespace Qualifiers

**Flagged on 11 PRs (0.8%). Active 2021–2026.**

Code inside `namespace itk { ... }` should not prefix ITK symbols with
`itk::`.

```cpp
// BAD — inside a .cxx file that is already in namespace itk:
itk::ConvertNumberToString(value);

// GOOD:
ConvertNumberToString(value);
```

---

## 13. AI-Generated Descriptions Must Be Factually Verified

**Flagged on 1 PR (2026). Severity: high — incorrect claims erode reviewer trust.**

AI-generated PR descriptions and review summaries have been observed
claiming incorrect counts (e.g., "three temporary variables eliminated"
when only one existed). This has been compared to known LLM counting
errors.

**Rule:** Before submitting an AI-generated description, manually verify
every concrete claim — counts, variable names, file paths, and behavioral
assertions. If the AI says "N items were changed," count them yourself.

### Keep commit messages and PR descriptions in sync with scope

Refactoring, squashing, addressing reviewer comments, and adding fixup
commits frequently change the scope of a PR. After any such change, AI
tools must re-read all commit messages and the PR title/body and verify
they still accurately describe what the PR does. Stale descriptions
that reference removed work, omit added work, or overstate the change
are a common source of reviewer confusion and erode trust in
AI-assisted PRs.

**Checklist after every scope change:**
- Does the PR title still describe the current change set?
- Does each commit message accurately reflect its diff?
- Were claims about "N files changed" or "M patterns fixed" invalidated
  by the scope change?
- If commits were squashed, does the squashed message cover everything
  that was folded in?

---

## Methodology

Generated 2026-04-12 by analyzing 8,484 inline review comments across
1,457 PRs (2017–2026) from the ITK GitHub repository. Topics counted
per distinct PR, not per comment. See the PR description for details.
