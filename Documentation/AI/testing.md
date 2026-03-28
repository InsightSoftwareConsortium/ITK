# ITK Testing

## Running Tests

```bash
cd build
ctest -j8                    # All tests in parallel
ctest -R ImageFilter         # Tests matching regex
ctest -L REQUIRES_GPU        # Tests with label
ctest --rerun-failed         # Only previously failed tests
ctest -L ITKCommon           # Tests for a specific module
```

Via Pixi (recommended):
```bash
pixi run --as-is test           # C++ tests
pixi run --as-is test-python    # Python tests
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
