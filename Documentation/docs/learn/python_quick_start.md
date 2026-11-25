# Python Quick Start

## Installation

To install the ITK Python package:

```bash
pip install itk
```

## Usage

### Basic example

Here is a simple Python script that reads an image, applies a median image filter (radius of 2 pixels), and writes the resulting image in a file.

```python
#!/usr/bin/env python3

import itk
import sys

input_filename = sys.argv[1]
output_filename = sys.argv[2]

image = itk.imread(input_filename)

median = itk.median_image_filter(image, radius=2)

itk.imwrite(median, output_filename)
```

### ITK and NumPy

A common use case for using ITK in Python is to mingle NumPy and ITK operations on raster data. ITK provides a large number of I/O image formats and several sophisticated image processing algorithms not available in any other packages. The ability to intersperse that with the SciPy ecosystem provides a great tool for rapid prototyping.

The following script shows how to integrate NumPy and `itk.Image`:

```python
import itk
import numpy as np

# Read input image
itk_image = itk.imread(input_image_filename)

# Run filters on itk.Image

# View only of itk.Image, pixel data is not copied
array_view = itk.array_view_from_image(itk_image)

# Copy of itk.Image, pixel data is copied
array_copy = itk.array_from_image(itk_image)
# Equivalent
array_copy = np.asarray(itk_image)

# Image metadata
# Sequences, e.g. spacing, are in zyx (NumPy) indexing order
metadata = dict(itk_image)

# Pixel array and image metadata together
# in standard Python data types + NumPy array
# Sequences, e.g. spacing, are in xyz (ITK) indexing order
image_dict = itk.dict_from_image(itk_image)


# Do interesting things...


# Convert back to ITK, view only, data is not copied
itk_image_view = itk.image_view_from_array(array_copy)

# Convert back to ITK, data is copied
itk_image_copy = itk.image_from_array(array_copy)

# Add the metadata
for k, v in metadata.items():
    itk_image_view[k] = v

# Save result
itk.imwrite(itk_image_view, output_image_filename)

# Convert back to itk image data structure
itk_image = itk.image_from_dict(image_dict)
```

ITK's `itk.Mesh` class also works seamlessly with NumPy:

```python
# Read input mesh
itk_mesh = itk.meshread(input_mesh_filename)

# Convert to standard Python data types + NumPy arrays for points, cells
mesh_dict = itk.dict_from_mesh(itk_mesh)


# Do interesting things...


# Convert back to itk mesh data structure
itk_mesh = itk.mesh_from_dict(mesh_dict)

# Save result
itk.meshwrite(itk_mesh, output_mesh_filename)
```

ITK's `itk.Transform` class also works seamlessly with NumPy:

```python
# Read input transforms
#
# This is a Python list
#
# When there is more than one transformation
# the list defines a transformation chain
itk_transforms = itk.transformread(input_transform_filename)

# Convert to standard Python data types + NumPy arrays
transform_dicts = [itk.dict_from_transform(t) for t in itk_transforms]


# Do interesting things...


# Convert back to itk transform instance
itk_transforms = [itk.transform_from_dict(t) for t in transform_dicts]

# Save result
itk.transformwrite(itk_transforms, output_transform_filename)
```

The `itk.Matrix`, VNL vectors, and VNL matrices can be converted back and forth with
their NumPy counterparts:

```python
# VNL matrix from np.ndarray
arr = np.zeros([3,3], np.uint8)
matrix = itk.vnl_matrix_from_array(arr)

# Array from VNL matrix
arr = itk.array_from_vnl_matrix(matrix)

# VNL vector from np.ndarray
vec = np.zeros([3], np.uint8)
vnl_vector = itk.vnl_vector_from_array(vec)

# Array from VNL vector
vec = itk.array_from_vnl_vector(vnl_vector)

# itk.Matrix from np.ndarray
mat = itk.matrix_from_array(np.eye(3))

# np.ndarray from itk.Matrix
arr = itk.array_from_matrix(mat)
# Equivalent
arr = np.asarray(mat)
```

### ITK and ITK-Wasm

[ITK-Wasm](https://wasm.itk.org) can be used with native `itk` Python bindings.

Both packages support common Python dictionary representations of the data structures used on interfaces. The non-dictionary types are more convenient to work with directly and provide strong typing for function calls.

## Convert from `itkwasm` to `itk`

To convert from an `itkwasm` dataclass interface type to a native `itk` Python type, first convert the `itkwasm` type to a dictionary, then use the `itk.<type>_from_dict` function. Example:

```python
import itk
from itkwasm import Image
from dataclasses import asdict

itkwasm_image = Image()
image_dict = asdict(itkwasm_image)

itk_image = itk.image_from_dict(image_dict)
```

## Convert from `itk` to `itkwasm`

To convert from a native `itk` Python type to an `itkwasm` dataclass interface type, first convert the `itkwasm` type to a dictionary the `itk.<type>_from_dict`, then pass the dictionary as keyword arguments to `itkwasm` constructor with the `**` operator. Example:


```python
import itk
from itkwasm import Image

# Create an itk.Image
itk_image = itk.Image.New()
itk_image.SetRegions([8,8])
itk_image.Allocate()
image_dict = itk.dict_from_image(itk_image)

itkwasm_image = Image(**image_dict)
```

## itkwasm file formats

`itkwasm` provides file formats corresponding to its interface types. These file formats keep Wasm module sizes tiny, enable efficient and one-to-one serialization, assist with debugging, and bridge with [Web3 technologies](https://en.wikipedia.org/wiki/Web3).

The file extensions for these formats are `.iwi` and `.iwm` for images and mesh-like data, respectively. When written, these will output directories with an `index.json` file and raw binary files. When `.iwi.cbor` or `.iwm.cbor` extensions are used, a single [CBOR](https://en.wikipedia.org/wiki/CBOR) file is created.

These file formats can also be used with native ITK Python.

Install the binary Python package:

```bash
pip install itk-webassemblyinterface
```

Then use with `itk.imread`, `itk.imwrite`, `itk.meshread`, `itk.meshwrite`. Example:

```python
import itk

image = itk.imread('cthead1.png')
itk.imwrite(image, 'cthead1.iwi')
itk.imwrite(image, 'cthead1.iwi.cbor')

mesh = itk.meshread('cow.vtk')
itk.meshwrite(mesh, 'cow.iwm')
itk.meshwrite(mesh, 'cow.iwm.cbor')
```

### ITK and Xarray

An `itk.Image` can be converted to and from an [`xarray.DataArray`](https://xarray.pydata.org/en/stable/generated/xarray.DataArray.html) while
preserving metadata:

```python
da = itk.xarray_from_image(image)

image = itk.image_from_xarray(da)
```

### ITK and VTK

An `itk.Image` can be converted to and from a [`vtk.vtkImageData`](https://vtk.org/doc/nightly/html/classvtkImageData.html) while
preserving metadata:

```python
vtk_image = itk.vtk_image_from_image(image)

image = itk.image_from_vtk_image(vtk_image)
```

### ITK and napari

An `itk.Image` can be converted to and from a [`napari.layers.Image`](https://napari.org/stable/api/napari.layers.Image.html#napari.layers.Image) while
preserving metadata with the [itk-napari-conversion package](https://github.com/InsightSoftwareConsortium/itk-napari-conversion).

### ITK Python types

| C++ type              | Python type          | NumPy dtype    |
| --------------------- | -------------------- | -------------- |
| `float`               | `itk.F`              | `np.float32`   |
| `double`              | `itk.D`              | `np.float64`   |
| `unsigned char`       | `itk.UC`             | `np.uint8`     |
| `std::complex<float>` | `itk.complex[itk.F]` | `np.complex64` |

This list is not exhaustive and is only presented to illustrate the type names. The complete list of types can be found in the [ITK Software Guide](https://itk.org/ItkSoftwareGuide.pdf).

Types can also be obtained from their name in the C programming language:

```python
itk.F == itk.ctype('float')  # True
```

To cast the pixel type of an image, use `.astype`:

```python
image = itk.imread(input_filename)

# Cast to an unsigned char pixel type
cast_image = image.astype(itk.UC)

# Equivalent
cast_image = image.astype(np.uint8)

itk.imwrite(cast_image, output_filename)
```

### Metadata dictionary

An `itk.Image` has a metadata dict of `key: value` pairs.

The metadata dictionary can be retrieved with:

```python
meta_dict = dict(image)
```

For example:

```python
In [3]: dict(image)
Out[3]:
{'0008|0005': 'ISO IR 100',
 '0008|0008': 'ORIGINAL\\PRIMARY\\AXIAL',
 '0008|0016': '1.2.840.10008.5.1.4.1.1.2',
 '0008|0018': '1.3.12.2.1107.5.8.99.484849.834848.79844848.2001082217554549',
 '0008|0020': '20010822',
```

Individual dictionary items can be accessed or assigned:

```python
print(image["0008|0008"])

image["origin"] = [4.0, 2.0, 2.0]
```

In the Python dictionary interface to image metadata, keys for the spatial
metadata, the *'origin'*, *'spacing'*, and *'direction'*, are reversed in
order from `image.GetOrigin()`, `image.GetSpacing()`, `image.GetDirection()`
to be consistent with the [NumPy array index order](https://scikit-image.org/docs/dev/user_guide/numpy_images.html#notes-on-the-order-of-array-dimensions)
resulting from pixel buffer array views on the image.

### Access pixel data with NumPy indexing

Array views of an `itk.Image` provide a way to set and get pixel values with NumPy indexing syntax, e.g.:

```python
In [6]: image[0,:2,4] = [5,5]

In [7]: image[0,:4,4:6]
Out[7]:
NDArrayITKBase([[    5,  -997],
                [    5, -1003],
                [ -993,  -999],
                [ -996,  -994]], dtype=int16)
```

### Input/Output (I/O)

Convenient functions are provided read and write from ITK's many supported
file formats:

```python
image = itk.imread("image.tif")

# Read in with a specific pixel type.
image = itk.imread("image.tif", itk.F)

# Read in an image series.
# Pass a sorted list of files.
image = itk.imread(["image1.png", "image2.png", "image3.png"])

# Read in a volume from a DICOM series.
# Pass a directory.
# Only a single series, sorted spatially, will be returned.
image = itk.imread("/a/dicom/directory/")

# Write an image.
itk.imwrite(image, "image.tif")


# Read a mesh.
mesh = itk.meshread("mesh.vtk")

# Write a mesh.
itk.meshwrite(mesh, "mesh.vtk")


# Read a spatial transform.
transform = itk.transformread("transform.h5")

# Write a spatial transform.
itk.transformwrite(transform, "transform.h5")
```

### Image filters and Image-like inputs and outputs

All `itk` functional image filters operate on an `itk.Image` but also:

- [xarray.DataArray](https://xarray.pydata.org/en/stable/generated/xarray.DataArray.html) \*
- [numpy.ndarray](https://numpy.org/doc/stable/reference/generated/numpy.ndarray.html)
- [dask.array.Array](https://docs.dask.org/en/latest/array.html)

\* Preserves image metadata

### Filter parameters

ITK filter parameters can be specified in the following ways:

```python
# Pythonic snake case keyword arguments:
#
#   number_of_iterations
#
smoothed = itk.anti_alias_binary_image_filter(image, number_of_iterations=3)

# CamelCase keyword arguments:
#
#   NumberOfIterations
#
smoother = itk.AntiAliasBinaryImageFilter.New(image, NumberOfIterations=3)
smoother.Update()
smoothed = smoother.GetOutput()

# CamelCase Set method:
#
#   SetNumberOfIterations
#
smoother = itk.AntiAliasBinaryImageFilter.New(image)
smoother.SetNumberOfIterations(3)
smoother.Update()
smoothed = smoother.GetOutput()
```

### Filter types

In `itk`, filters are optimized at compile time for each image pixel type and
image dimension. There are two ways to instantiate these filters with the `itk`
Python wrapping:

- *Implicit (recommended)*: Type information is automatically detected from the data. Typed filter objects and images are implicitly created.

```python
image = itk.imread(input_filename)

# Use ITK's functional, Pythonic interface. The filter type is implied by the
# type of the input image. The filter is eagerly executed, and the output image
# is directly returned.
smoothed = itk.median_image_filter(image)

# Alternatively, create filter objects. These filter objects can be connected in
# a pipeline to stream-process large datasets. To generate the output of the
# pipeline, .Update() must explicitly be called on the last filter of the
# pipeline.
#
# We can implicitly instantiate the filter object based on the type
# of the input image in multiple ways.

# Use itk.ImageFileReader instead of the wrapping function,
# itk.imread to illustrate this example.
ImageType = itk.Image[itk.UC, 2]
reader = itk.ImageFileReader[ImageType].New(FileName=input_filename)
# Here we specify the filter input explicitly
median = itk.MedianImageFilter.New(Input=reader.GetOutput())
# Same as above but shortened. Input does not have to be specified.
median = itk.MedianImageFilter.New(reader.GetOutput())
# Same as above. .GetOutput() does not have to be specified.
median = itk.MedianImageFilter.New(reader)

median.Update()
smoothed = median.GetOutput()
```

- *Explicit*: This can be useful if an appropriate type cannot be determined implicitly or when a different filter type than the default is desired.

To specify the type of the filter, use the `ttype` keyword argument. Explicit instantiation of a median image filter:

```python
# An apriori ImageType
PixelType = itk.F
ImageType = itk.Image[PixelType,2]
image = itk.imread(input_filename, PixelType)

# An image type dynamically determined from the type on disk
image = itk.imread(input_filename)
ImageType = type(image)

# Functional interface
# The `ttype` keyword argument specifies the filter type.
smoothed = itk.median_image_filter(image, ttype=(ImageType, ImageType))

# Object-oriented interface
reader = itk.ImageFileReader[ImageType].New(file_name=input_filename)
median = itk.MedianImageFilter[ImageType, ImageType].New()
median.SetInput(reader.GetOutput())
median.Update()
smoothed = median.GetOutput()
```

### Instantiate an ITK object

There are two types of ITK objects. Most ITK objects, such as images, filters, or adapters, are instantiated the following way:

```python
InputType = itk.Image[itk.F,3]
OutputType = itk.Image[itk.F,3]
median = itk.MedianImageFilter[InputType, OutputType].New()
```

Some objects, like a Matrix, Vector, or RGBPixel, do not require the attribute `.New()` to be added to instantiate them:

```python
pixel = itk.RGBPixel[itk.UC]()
```

In case of doubt, look at the attributes of the object you are trying to instantiate.

## Examples

Examples can be found in the [ITKSphinxExamples project](https://examples.itk.org/).
