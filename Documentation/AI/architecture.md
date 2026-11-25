# ITK Architecture

## Module Structure

ITK uses a module-based architecture. Each module is a self-contained unit with
declared dependencies via `itk-module.cmake`:

```cmake
itk_module(ITKCommon
  DEPENDS ITKEigen3 ITKVNL
  PRIVATE_DEPENDS ITKDoubleConversion
  COMPILE_DEPENDS ITKKWSys
  TEST_DEPENDS ITKTestKernel ITKMesh
  DESCRIPTION "Core ITK classes..."
)
```

Modules are organized in:
- `Modules/Core/` — Image, Region, Point, Vector, pipeline infrastructure
- `Modules/Filtering/` — Image processing filters
- `Modules/IO/` — JPEG, PNG, NIFTI, DICOM, and other format I/O
- `Modules/Registration/` — Image registration algorithms
- `Modules/Segmentation/` — Segmentation algorithms
- `Modules/Numerics/` — Optimization and numerical methods
- `Modules/ThirdParty/` — Vendored deps (HDF5, JPEG, TIFF, VNL, Eigen3); managed as git subtrees
- `Modules/Bridge/` — Bridges to VTK, NumPy
- `Modules/Remote/` — External modules fetched at build time

## Template-Heavy C++ Design

ITK uses C++ templates for dimension-agnostic (2D/3D/nD) and pixel-type-agnostic
code. Both are compile-time parameters:

```cpp
itk::Image<TPixel, VImageDimension>
```

Compile-time polymorphism is achieved via traits and policy classes rather than
virtual dispatch.

## Pipeline Architecture

ITK uses a lazy evaluation pipeline:
- **Process objects** (`itk::ProcessObject`) — filters that transform data
- **Data objects** (`itk::DataObject`) — `itk::Image`, meshes, etc.
- Filters connect via `SetInput()` / `GetOutput()` and execute only when `Update()` is called
- `itk::SmartPointer<T>` handles all memory management — never call `delete` on ITK objects

## Python Wrapping

Wrappers are generated via SWIG. Not all C++ types are wrapped. To check
available instantiations, look in `wrapping/` subdirectories of each module.

Common wrapped pixel types: `F` (float), `D` (double), `UC` (unsigned char), `US` (unsigned short).

Snake_case Python API mirrors C++ (e.g., `itk.median_image_filter()`).

```python
import itk
image = itk.imread('input.png')
smoothed = itk.median_image_filter(image, radius=2)
itk.imwrite(smoothed, 'output.png')
```
