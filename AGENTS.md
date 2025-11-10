# ITK AI Agent Guide

ITK (Insight Toolkit) is a cross-platform, open-source toolkit for N-dimensional scientific image processing, segmentation, and registration. This guide helps AI agents navigate ITK's unique architecture and development workflows.

## Architecture Overview

### Modular Structure
ITK uses a **module-based architecture** where each module is a self-contained unit with dependencies. Modules are organized in:
- `Modules/Core/` - Essential classes (Image, Region, Point, Vector, pipeline infrastructure)
- `Modules/Filtering/` - Image processing filters
- `Modules/IO/` - Image, mesh, and transform I/O for various formats (JPEG, PNG, NIFTI, DICOM, etc.)
- `Modules/Registration/` - Image registration algorithms
- `Modules/Segmentation/` - Segmentation algorithms
- `Modules/Numerics/` - Optimization and numerical methods
- `Modules/ThirdParty/` - Vendored dependencies (HDF5, JPEG, TIFF, VNL, Eigen3)
- `Modules/Bridge/` - Bridges to VTK, NumPy
- `Modules/Remote/` - External modules fetched during build

Each module has an `itk-module.cmake` file declaring dependencies:
```cmake
itk_module(ITKCommon
  DEPENDS ITKEigen3 ITKVNL
  PRIVATE_DEPENDS ITKDoubleConversion
  COMPILE_DEPENDS ITKKWSys
  TEST_DEPENDS ITKTestKernel ITKMesh
  DESCRIPTION "Core ITK classes..."
)
```

### Template-Heavy C++ Design
ITK extensively uses **C++ templates** and **generic programming** for:
- Dimension-agnostic code (2D, 3D, nD images)
- Type flexibility (pixel types: unsigned char, float, RGB, etc.)
- Compile-time polymorphism via traits and policy classes

Example: `itk::Image<TPixel, VImageDimension>` where both parameters are compile-time constants.

### Pipeline Architecture
ITK uses a **data pipeline** pattern:
- **Process objects** (`itk::ProcessObject`, e.g. most filters) perform computations
- **Data objects** (`itk::DataObject`, e.g., `itk::Image`) store data
- Filters connect via `SetInput()` / `GetOutput()` and execute lazily with `Update()`
- Smart pointers (`itk::SmartPointer<T>`) handle memory management

### Python Wrapping
ITK provides **Python wrappers** via SWIG:
- Wrapping configs in `Wrapping/` directory
- Explicit template instantiations in `wrapping/` subdirectories of modules
- Python package structure mirrors C++ modules
- Snake_case functions available (e.g., `itk.median_image_filter()`)
- Install via `pip install itk` or build with `ITK_WRAP_PYTHON=ON`

## Build System

### CMake Configuration
ITK requires CMake 3.22.1+. Key build options:

**Essential:**
```bash
cmake -B build -S . \
  -DCMAKE_BUILD_TYPE=Release \
  -DITK_BUILD_DEFAULT_MODULES=ON \
  -DBUILD_TESTING=ON \
  -DBUILD_EXAMPLES=OFF
```

**Python wrapping:**
```bash
cmake -B build-python -S . \
  -DITK_WRAP_PYTHON=ON \
  -DITK_WRAP_unsigned_short=ON \
  -DITK_WRAP_IMAGE_DIMS="2;3;4"
```

**Module selection:**
- `Module_<ModuleName>=ON` to enable specific modules
- `ITK_BUILD_DEFAULT_MODULES=ON` builds standard modules
- Use `find_package(ITK COMPONENTS ITKCommon ITKIOImageBase)` in external projects

### Building
```bash
# Build via Pixi (recommended for development)
pixi run --as-is build         # Build C++ tests
pixi run --as-is build-python  # Build Python tests

# Run tests via Pixi (recommended for development)
pixi run --as-is test         # C++ tests
pixi run --as-is test-python  # Python tests
```

For an interactive shell with ITK environment:
```bash
pixi shell -e cxx    # C++ development
pixi shell -e python # Python development
```

### Module Dependencies
ITK automatically resolves module dependencies via CMake. The module DAG is loaded from `itk-module.cmake` files. To find required modules for code:
```bash
python Utilities/Maintenance/WhatModulesITK.py /path/to/ITK/source file1.cxx file2.h
```

## Testing

### CTest Integration
Tests use **CTest** framework:
```bash
cd build
ctest -j8                    # Run all tests in parallel
ctest -R ImageFilter         # Run tests matching regex
ctest -L REQUIRES_GPU        # Run tests with label
ctest --rerun-failed         # Rerun only failed tests
```

### Test Organization
Each module has a `test/` directory with:
- **CTest tests**: Defined via `itk_add_test()` macro
- **GTest tests (preferred)**: Modern C++ tests using `creategoogletestdriver()`
- **Baseline images**: Stored via `ExternalData` (downloaded on demand)

Example test definition:
```cmake
itk_add_test(NAME itkImageTest
  COMMAND ITKCommonTestDriver itkImageTest
    DATA{Input/image.png}
    ${ITK_TEST_OUTPUT_DIR}/output.png
)
```

### ExternalData System
Large test data is **not stored in Git**. Instead, ITK uses `ExternalData` with content hashes:
- `DATA{path/to/file.png}` references data by hash
- Data downloaded from `https` resources during build
- Test data uploaded separately for new tests

## Development Workflow

### Setup for Development
**First time only:**
```bash
./Utilities/SetupForDevelopment.sh
```
This configures:
- Git hooks (pre-commit, commit-msg)
- clang-format integration (auto-formats C++ on commit)
- KWStyle configuration
- GitHub remote setup

### Code Style

**Automatic C++ formatting:**
ITK enforces `.clang-format` style automatically via pre-commit hook. To format manually:
```bash
Utilities/Maintenance/clang-format.bash --modified  # Format modified files
```

**Key style rules:**
- Use `clang-format` 19.1.7 (enforced by pre-commit)
- `constexpr` instead of `#define` for constants
- Smart pointers for all ITK objects: `auto image = ImageType::New();`
- American English spelling
- Doxygen comments with `\` (backslash) style: `\class`, `\brief`
- No `using namespace` in headers

**Naming conventions:**
- Classes: `PascalCase` (e.g., `MedianImageFilter`)
- Variables: `camelCase` or `lowercase` with no underscores
- Member variables: `m_MemberVariable` prefix
- Template parameters: `TInputImage`, `TOutputImage` prefix
- Macros: `ITK_UPPERCASE_MACRO`

### Commit Guidelines
**Required format** (enforced by `kw-commit-msg.py` hook):
```
PREFIX: Brief description (≤78 chars)

Longer explanation if needed. Reference issues or features.
```

Common prefixes: `ENH:`, `BUG:`, `COMP:`, `DOC:`, `STYLE:`, `PERF:`, `WIP:`

### CI/CD
ITK uses:
- **Azure Pipelines** for Linux, Windows, macOS builds (C++ and Python)
- **GitHub Actions** for Pixi builds and Apple Silicon
- **CDash** dashboards at https://open.cdash.org/index.php?project=Insight

## Key Conventions

### ITK-Specific Patterns

**Object creation:**
```cpp
auto filter = FilterType::New();  // Factory method, returns SmartPointer
filter->SetInput(image);
filter->Update();  // Lazy evaluation
auto output = filter->GetOutput();
```

**Image iteration:**
Use **iterators** (not raw buffers) for image traversal:
```cpp
itk::ImageRegionIterator<ImageType> it(image, region);
for (; !it.IsAtEnd(); ++it) {
  it.Set(it.Get() * 2);
}
```

**Macros for class boilerplate:**
```cpp
using Self = MyClass;
using Superclass = BaseClass;
using Pointer = SmartPointer<Self>;
itkNewMacro(Self);  // Provides New() method
itkTypeMacro(Self, Superclass);  // RTTI support
itkSetMacro(Radius, unsigned int);  // Generates SetRadius()
itkGetConstMacro(Radius, unsigned int);  // Generates GetRadius()
```

### File Organization
Each module follows:
```
ModuleName/
├── itk-module.cmake          # Module metadata
├── include/                  # Public headers (.h)
│   ├── itkClassName.h
│   └── itkClassName.hxx      # Template implementations
├── src/                      # Non-template implementations (.cxx)
│   └── itkClassName.cxx      # Non-template implementations
├── test/                     # Tests
│   ├── CMakeLists.txt
│   └── itkClassNameTest.cxx
└── wrapping/                 # Python wrapping (if applicable)
    └── itkClassName.wrap
```

**Header structure:**
- `.h` files: Class declarations
- `.hxx` files: Template method implementations (included at end of `.h`)
- `.cxx` files: Non-template implementations compiled into libraries

### Third-Party Code
Third-party libraries in `Modules/ThirdParty/` are **subtrees**, not submodules:
- Updated via `git subtree pull`
- Maintained upstream separately
- Wrapped with ITK CMake logic

## External Module Development

To create a remote module:
1. Create `itk-module.cmake` with dependencies
2. Create `MyModule.remote.cmake` in ITK's `Modules/Remote/`
3. Use `itk_module_impl()` macro in module's CMakeLists.txt
4. Test as external build:
```bash
cmake -B build-external -S path/to/module \
  -DITK_DIR=/path/to/ITK-build
```

## Common Pitfalls

1. **Template compilation errors**: ITK's heavy template use causes verbose errors. Focus on the **first error** in the output.

2. **Python wrapping**: Not all C++ types are wrapped. Check `wrapping/` directories for available instantiations. Common wrapped types: `F` (float), `D` (double), `UC` (unsigned char), `US` (unsigned short).

3. **Memory management**: Always use `SmartPointer`. Never `delete` ITK objects manually.

4. **Update() calls**: Filters don't execute until `Update()` is called. Changes to filter parameters after `Update()` require another `Update()`.

5. **Module dependencies**: If code fails to link, check `itk-module.cmake` dependencies. Use `DEPENDS` for public deps, `PRIVATE_DEPENDS` for implementation deps.

## Resources

- Main docs: https://docs.itk.org/
- Discourse forum: https://discourse.itk.org/
- Software Guide: https://itk.org/ItkSoftwareGuide.pdf
- Examples: https://examples.itk.org/
- Doxygen API: https://itk.org/Doxygen/html/
- Contribution guide: https://docs.itk.org/en/latest/contributing/

## Quick Reference

**Find class documentation:**
```bash
# Search Doxygen locally after building docs
cmake -DITK_BUILD_DOCUMENTATION=ON ..
```

**Test a single module:**
```bash
ctest -L ITKCommon
```

**Build only specific modules:**
```cmake
set(Module_ITKCommon ON)
set(Module_ITKIOImageBase ON)
set(ITK_BUILD_DEFAULT_MODULES OFF)
```

**Python quick test:**
```python
import itk
image = itk.imread('input.png')
smoothed = itk.median_image_filter(image, radius=2)
itk.imwrite(smoothed, 'output.png')
```
