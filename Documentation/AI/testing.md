# ITK Testing

## Running Tests

Via Pixi (recommended — works even without system-level cmake/ctest):
```bash
pixi run --as-is test                        # All C++ tests
pixi run --as-is test-python                 # Python tests
pixi run --as-is ctest -j8                   # All tests in parallel
pixi run --as-is ctest -R ImageFilter        # Tests matching regex
pixi run --as-is ctest -L REQUIRES_GPU       # Tests with label
pixi run --as-is ctest --rerun-failed        # Only previously failed tests
pixi run --as-is ctest -L ITKCommon          # Tests for a specific module
```

## Test Organization

Each module has a `test/` directory. Tests are declared in `test/CMakeLists.txt`.

**GTest (preferred for new tests):**
```cmake
creategoogletestdriver(ITKCommonGTests "${ITKCommon-Test_LIBRARIES}"
  itkFooGTest.cxx
)
```

**Legacy CTest:**
```cmake
itk_add_test(NAME itkImageTest
  COMMAND ITKCommonTestDriver itkImageTest
    DATA{Input/image.png}
    ${ITK_TEST_OUTPUT_DIR}/output.png
)
```

New tests should use GoogleTest (`itkFooGTest.cxx`) rather than the legacy
`itkFooTest.cxx` + `itk_add_test()` pattern. See `.claude/commands/convert-to-gtest.md`
for the conversion workflow.

## ExternalData

Large test data is not stored in Git. References use content hashes:
```cmake
DATA{Input/image.png}
```
Data is downloaded from content-addressed storage during build. To add new
test data, upload the file and commit its hash file. See
`Testing/Data/README.md` for details.

## Key GTest Macros

```cpp
// Exercise basic object interface (New, Print, etc.)
// `ptr` must be a named variable, NOT an expression
ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(ptr, ClassName, SuperclassName);

// Set/Get boolean member variable
ITK_GTEST_SET_GET_BOOLEAN((&obj), VariableName, value);
```

## Exception Tests in GTest Files

The legacy `ITK_TRY_EXPECT_EXCEPTION` macro from `itkTestingMacros.h` uses
`return EXIT_FAILURE` internally and is **incompatible with GoogleTest**'s
`void TestBody()` — using it produces:

```
error: return-statement with a value, in function returning ‘void’
```

In a `*GTest.cxx` file, use vanilla GoogleTest exception macros instead:

```cpp
// BAD — does not compile in a GTest TEST() body:
ITK_TRY_EXPECT_EXCEPTION(filter->Update());

// GOOD:
EXPECT_THROW(filter->Update(), itk::ExceptionObject);
ASSERT_THROW(filter->Update(), itk::ExceptionObject);
```

The `ITK_TRY_EXPECT_*` family is still appropriate in legacy
`itkXxxTest.cxx` files that have an `int main()` returning `EXIT_FAILURE`
on error, but never in a Google Test `TEST(...)` block.

**References:** PR #6034 (StructuralSimilarityImageFilter GTest).
