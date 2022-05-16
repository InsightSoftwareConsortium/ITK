# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

# also test the import callback feature

import sys
import os
import numpy as np
import pathlib
import pickle

import itk


def custom_callback(name, progress):
    if progress == 0:
        print(f"Loading {name}...", file=sys.stderr)
    if progress == 1:
        print("done", file=sys.stderr)


import itkConfig

itkConfig.ImportCallback = custom_callback

# test setting the number of threads
itk.set_nthreads(4)
assert itk.get_nthreads() == 4

# test the force load function
itk.force_load()


filename = sys.argv[1]
mesh_filename = sys.argv[2]
transform_filename = sys.argv[3]

PixelType = itk.UC
dim = 2
ImageType = itk.Image[PixelType, dim]
ReaderType = itk.ImageFileReader[ImageType]
reader = ReaderType.New(FileName=filename)

# test snake_case keyword arguments
reader = ReaderType.New(file_name=filename)

# test echo
itk.echo(reader)
itk.echo(reader, sys.stdout)

# test class_
assert itk.class_(reader) == ReaderType
assert itk.class_("dummy") == str

# test template
assert itk.template(ReaderType) == (itk.ImageFileReader, (ImageType,))
assert itk.template(reader) == (itk.ImageFileReader, (ImageType,))
try:
    itk.template(str)
    raise Exception("unknown class should send an exception")
except KeyError:
    pass

# test ctype
assert itk.ctype("unsigned short") == itk.US
assert itk.ctype("        unsigned      \n   short \t  ") == itk.US
assert itk.ctype("signed short") == itk.SS
assert itk.ctype("short") == itk.SS
try:
    itk.ctype("dummy")
    raise Exception("unknown C type should send an exception")
except KeyError:
    pass


# test output
assert itk.output(reader) == reader.GetOutput()
assert itk.output(1) == 1
# test the deprecated image
assert itk.image(reader) == reader.GetOutput()
assert itk.image(1) == 1

# test size
s = itk.size(reader)
assert s[0] == s[1] == 256
s = itk.size(reader.GetOutput())
assert s[0] == s[1] == 256

# test physical size
s = itk.physical_size(reader)
assert s[0] == s[1] == 256.0
s = itk.physical_size(reader.GetOutput())
assert s[0] == s[1] == 256.0

# test spacing
s = itk.spacing(reader)
assert s[0] == s[1] == 1.0
s = itk.spacing(reader.GetOutput())
assert s[0] == s[1] == 1.0

# test origin
s = itk.origin(reader)
assert s[0] == s[1] == 0.0
s = itk.origin(reader.GetOutput())
assert s[0] == s[1] == 0.0

# test index
s = itk.index(reader)
assert s[0] == s[1] == 0
s = itk.index(reader.GetOutput())
assert s[0] == s[1] == 0

# test region
s = itk.region(reader)
assert s.GetIndex()[0] == s.GetIndex()[1] == 0
assert s.GetSize()[0] == s.GetSize()[1] == 256
s = itk.region(reader.GetOutput())
assert s.GetIndex()[0] == s.GetIndex()[1] == 0
assert s.GetSize()[0] == s.GetSize()[1] == 256

# test range
assert itk.range(reader) == (0, 255)
assert itk.range(reader.GetOutput()) == (0, 255)


# test write
itk.imwrite(reader, sys.argv[4])
itk.imwrite(reader, sys.argv[4], imageio=itk.PNGImageIO.New())
itk.imwrite(reader, sys.argv[4], True)

# test read
image = itk.imread(pathlib.Path(filename))
assert type(image) == itk.Image[itk.RGBPixel[itk.UC], 2]
image = itk.imread(filename, itk.F)
assert type(image) == itk.Image[itk.F, 2]
image = itk.imread(filename, itk.F, fallback_only=True)
assert type(image) == itk.Image[itk.RGBPixel[itk.UC], 2]
try:
    image = itk.imread(filename, fallback_only=True)
    # Should never reach this point if test passes since an exception
    # is expected.
    raise Exception("`itk.imread()` fallback_only should have failed")
except Exception as e:
    if str(e) == "pixel_type must be set when using the fallback_only option":
        pass
    else:
        raise e
image = itk.imread(filename, imageio=itk.PNGImageIO.New())
assert type(image) == itk.Image[itk.RGBPixel[itk.UC], 2]

# Test serialization with pickle
array = np.random.randint(0, 256, (8, 12)).astype(np.uint8)
image = itk.image_from_array(array)
image.SetSpacing([1.0, 2.0])
image.SetOrigin([11.0, 4.0])
theta = np.radians(30)
cosine = np.cos(theta)
sine = np.sin(theta)
rotation = np.array(((cosine, -sine), (sine, cosine)))
image.SetDirection(rotation)
serialize_deserialize = pickle.loads(pickle.dumps(image))
# verify_input_information checks origin, spacing, direction consistency
comparison = itk.comparison_image_filter(
    image, serialize_deserialize, verify_input_information=True
)
assert np.sum(comparison) == 0.0

# Make sure we can read unsigned short, unsigned int, and cast
image = itk.imread(filename, itk.UI)
assert type(image) == itk.Image[itk.UI, 2]
image = itk.imread(filename, itk.SI)
assert type(image) == itk.Image[itk.SI, 2]
as_float = image.astype(np.float32)
assert type(as_float) == itk.Image[itk.F, 2]

# test mesh read / write
mesh = itk.meshread(mesh_filename)
assert type(mesh) == itk.Mesh[itk.F, 3]
mesh = itk.meshread(mesh_filename, itk.UC)
assert type(mesh) == itk.Mesh[itk.UC, 3]
mesh = itk.meshread(mesh_filename, itk.UC, fallback_only=True)
assert type(mesh) == itk.Mesh[itk.F, 3]

itk.meshwrite(mesh, sys.argv[5])
itk.meshwrite(mesh, sys.argv[5], compression=True)

# test search
res = itk.search("Index")
assert res[0] == "Index"
assert res[1] == "index"
assert "ContinuousIndex" in res

res = itk.search("index", True)
assert "Index" not in res


# test down_cast
obj = itk.Object.cast(reader)
# be sure that the reader is casted to itk::Object
assert obj.__class__ == itk.Object
down_casted = itk.down_cast(obj)
assert down_casted == reader
assert down_casted.__class__ == ReaderType

# test setting the IO manually
png_io = itk.PNGImageIO.New()
assert png_io.GetFileName() == ""
reader = itk.ImageFileReader.New(FileName=filename, ImageIO=png_io)
reader.Update()
assert png_io.GetFileName() == filename

# test reading image series
series_reader = itk.ImageSeriesReader.New(FileNames=[filename, filename])
series_reader.Update()
assert series_reader.GetOutput().GetImageDimension() == 3
assert series_reader.GetOutput().GetLargestPossibleRegion().GetSize()[2] == 2

# test reading image series and check that dimension is not increased if
# last dimension is 1.
image_series = itk.Image[itk.UC, 3].New()
image_series.SetRegions([10, 7, 1])
image_series.Allocate()
image_series.FillBuffer(0)
image_series3d_filename = os.path.join(sys.argv[6], "image_series_extras_py.mha")
itk.imwrite(image_series, image_series3d_filename)
series_reader = itk.ImageSeriesReader.New(
    FileNames=[image_series3d_filename, image_series3d_filename]
)
series_reader.Update()
assert series_reader.GetOutput().GetImageDimension() == 3

# test reading image series with itk.imread()
image_series = itk.imread([pathlib.Path(filename), pathlib.Path(filename)])
assert image_series.GetImageDimension() == 3

# Numeric series filename generation without any integer index. It is
# only to produce an ITK object that users could set as an input to
# `itk.ImageSeriesReader.New()` or `itk.imread()` and test that it works.
numeric_series_filename = itk.NumericSeriesFileNames.New()
numeric_series_filename.SetStartIndex(0)
numeric_series_filename.SetEndIndex(3)
numeric_series_filename.SetIncrementIndex(1)
numeric_series_filename.SetSeriesFormat(filename)
image_series = itk.imread(numeric_series_filename.GetFileNames())
number_of_files = len(numeric_series_filename.GetFileNames())
assert image_series.GetImageDimension() == 3
assert image_series.GetLargestPossibleRegion().GetSize()[2] == number_of_files

# test reading image series with `itk.imread()` and check that dimension is
# not increased if last dimension is 1.
image_series = itk.imread([image_series3d_filename, image_series3d_filename])
assert image_series.GetImageDimension() == 3

baseline_parameters = np.array(
    [
        0.6563149,
        0.58065837,
        -0.48175367,
        -0.74079868,
        0.37486398,
        -0.55739959,
        -0.14306664,
        0.72271215,
        0.67617978,
        -66.0,
        69.0,
        32.0,
    ]
)
baseline_fixed_parameters = np.array([0.0, 0.0, 0.0])

# test transform read / write
transforms = itk.transformread(transform_filename)
fixed_parameters = np.asarray(transforms[0].GetFixedParameters())
parameters = np.asarray(transforms[0].GetParameters())
assert np.allclose(fixed_parameters, baseline_fixed_parameters)
assert np.allclose(parameters, baseline_parameters)

additional_transform = itk.TranslationTransform[itk.D, 3].New()
baseline_additional_transform_params = [3.0, 2.0, 8.0]
parameters = additional_transform.GetParameters()
parameters[0] = baseline_additional_transform_params[0]
parameters[1] = baseline_additional_transform_params[1]
parameters[2] = baseline_additional_transform_params[2]
additional_transform.SetParameters(parameters)
transforms.insert(0, additional_transform)
itk.transformwrite(transforms, sys.argv[7], compression=True)
transforms = itk.transformread(sys.argv[7])
fixed_parameters = np.asarray(transforms[1].GetFixedParameters())
parameters = np.asarray(transforms[1].GetParameters())
assert np.allclose(fixed_parameters, baseline_fixed_parameters)
assert np.allclose(parameters, baseline_parameters)
parameters = np.asarray(transforms[0].GetParameters())
assert np.allclose(parameters, np.array(baseline_additional_transform_params))

# pipeline, auto_pipeline and templated class are tested in other files

# BridgeNumPy

# Images

image = itk.imread(filename)
arr = itk.array_from_image(image)
arr.fill(1)
assert np.any(arr != itk.array_from_image(image))
arr = itk.array_from_image(image)
arr.fill(1)
assert np.any(arr != itk.array_from_image(image))
view = itk.GetArrayViewFromImage(image)
view.fill(1)
assert np.all(view == itk.array_from_image(image))
image = itk.image_from_array(arr)
image.FillBuffer(2)
assert np.any(arr != itk.array_from_image(image))
image = itk.GetImageViewFromArray(arr)
image.FillBuffer(2)
assert np.all(arr == itk.array_from_image(image))
arr_fortran = arr.copy(order="F")
image = itk.GetImageViewFromArray(arr_fortran)
assert np.array_equal(arr_fortran.shape, image.shape)
image = itk.image_from_array(arr, is_vector=True)
assert image.GetImageDimension() == 2
image = itk.GetImageViewFromArray(arr, is_vector=True)
assert image.GetImageDimension() == 2
arr = np.array([[1, 2, 3], [4, 5, 6]]).astype(np.uint8)
assert arr.shape[0] == 2
assert arr.shape[1] == 3
assert arr[1, 1] == 5
image = itk.image_from_array(arr)
assert np.array_equal(arr.shape, image.shape)
arrKeepAxes = itk.array_from_image(image, keep_axes=True)
assert arrKeepAxes.shape[0] == 3
assert arrKeepAxes.shape[1] == 2
assert arrKeepAxes[1, 1] == 4
arr = itk.array_from_image(image, keep_axes=False)
assert arr.shape[0] == 2
assert arr.shape[1] == 3
assert arr[1, 1] == 5
arrKeepAxes = itk.GetArrayViewFromImage(image, keep_axes=True)
assert arrKeepAxes.shape[0] == 3
assert arrKeepAxes.shape[1] == 2
assert arrKeepAxes[1, 1] == 4
arr = itk.GetArrayViewFromImage(image, keep_axes=False)
assert arr.shape[0] == 2
assert arr.shape[1] == 3
assert arr[1, 1] == 5
arr = arr.copy()
image = itk.image_from_array(arr)
image2 = type(image).New()
image2.Graft(image)
del image  # Delete image but pixel data should be kept in img2
image = itk.image_from_array(arr + 1)  # Fill former memory if wrongly released
assert np.array_equal(arr, itk.GetArrayViewFromImage(image2))
image2.SetPixel(
    [0] * image2.GetImageDimension(), 3
)  # For mem check in dynamic analysis
# VNL Vectors
v1 = itk.vnl_vector.D(2)
v1.fill(1)
v_np = itk.GetArrayFromVnlVector(v1)
assert v1.get(0) == v_np[0]
v_np[0] = 0
assert v1.get(0) != v_np[0]
view = itk.GetArrayViewFromVnlVector(v1)
assert v1.get(0) == view[0]
view[0] = 0
assert v1.get(0) == view[0]
# VNL Matrices
m1 = itk.vnl_matrix.D(2, 2)
m1.fill(1)
m_np = itk.GetArrayFromVnlMatrix(m1)
assert m1.get(0, 0) == m_np[0, 0]
m_np[0, 0] = 0
assert m1.get(0, 0) != m_np[0, 0]
view = itk.GetArrayViewFromVnlMatrix(m1)
assert m1.get(0, 0) == view[0, 0]
view[0, 0] = 0
assert m1.get(0, 0) == view[0, 0]
arr = np.zeros([3, 3])
m_vnl = itk.GetVnlMatrixFromArray(arr)
assert m_vnl(0, 0) == 0
m_vnl.put(0, 0, 3)
assert m_vnl(0, 0) == 3
assert arr[0, 0] == 0
# ITK Matrix
arr = np.zeros([3, 3], float)
m_itk = itk.GetMatrixFromArray(arr)
# Test snake case function
m_itk = itk.matrix_from_array(arr)
m_itk.SetIdentity()
# Test that the numpy array has not changed,...
assert arr[0, 0] == 0
# but that the ITK matrix has the correct value.
assert m_itk(0, 0) == 1
arr2 = itk.GetArrayFromMatrix(m_itk)
# Check that snake case function also works
arr2 = itk.array_from_matrix(m_itk)
# Check that the new array has the new value.
assert arr2[0, 0] == 1
arr2[0, 0] = 2
# Change the array value,...
assert arr2[0, 0] == 2
# and make sure that the matrix hasn't changed.
assert m_itk(0, 0) == 1
# Test __repr__
assert (
    repr(m_itk) == "itkMatrixD33 ([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]])"
)
# Test __array__
assert np.array_equal(np.asarray(m_itk), np.eye(3))
# Test array like input
arr3 = [[0.0, 0.0], [0.0, 0.0]]
m_itk = itk.matrix_from_array(arr3)
m_itk.SetIdentity()
# Test that the array like has not changed,...
assert arr3[0][0] == 0
# but that the ITK matrix has the correct value.
assert m_itk(0, 0) == 1

# test .astype for itk.Image
numpyImage = np.random.randint(0, 256, (8, 12, 5)).astype(np.uint8)
image = itk.image_from_array(numpyImage, is_vector=False)
assert type(image) == type(itk.image_from_array(numpyImage, ttype=(type(image),)))
assert type(image) == type(itk.image_from_array(numpyImage, ttype=[type(image)]))
assert type(image) == type(itk.image_from_array(numpyImage, ttype=type(image)))
cast = image.astype(np.uint8)
assert cast == image
(input_image_template, (input_pixel_type, input_image_dimension)) = itk.template(image)
assert hasattr(itk.CastImageFilter, "IUC3IF3")
for t in [
    [itk.F, np.float32, "IUC3IF3"],
    [itk.SS, np.int16, "IUC3ISS3"],
    [itk.UI, np.uint32, "IUC3IUI3"],
    [np.float32, np.float32, "IUC3IF3"],
]:
    if hasattr(itk.CastImageFilter, t[2]):
        cast = image.astype(t[0])
        (cast_image_template, (cast_pixel_type, cast_image_dimension)) = itk.template(
            cast
        )
        assert (
            cast_image_template == input_image_template
            and cast_image_dimension == input_image_dimension
            and cast.dtype == t[1]
        )

# test .astype for itk.VectorImage
numpyImage = np.random.randint(0, 256, (8, 5, 3)).astype(np.float32)
image = itk.image_from_array(numpyImage, is_vector=True)
assert type(image) == type(itk.image_from_array(numpyImage, ttype=(type(image),)))
assert type(image) == type(itk.image_from_array(numpyImage, ttype=[type(image)]))
assert type(image) == type(itk.image_from_array(numpyImage, ttype=type(image)))
ImageVectorsType = itk.Image[itk.Vector[itk.F, 3], 2]
imagevectors = itk.cast_image_filter(Input=image, ttype=(type(image), ImageVectorsType))
assert type(imagevectors) == ImageVectorsType
cast = image.astype(np.float32)
assert cast == image
(vector_image_template, (vector_pixel_type, vector_image_dimension)) = itk.template(
    image
)
for t in [
    [itk.D, np.float64, "VIF2VID2"],
    [itk.SS, np.int16, "VIF2VISS2"],
    [itk.UI, np.uint32, "VIF2VIUI2"],
    [np.float64, np.float64, "VIF2VID2"],
]:
    if hasattr(itk.CastImageFilter, t[2]):
        cast = image.astype(t[0])
        (cast_image_template, (cast_pixel_type, cast_image_dimension)) = itk.template(
            cast
        )
        assert (
            cast_image_template == vector_image_template
            and cast_image_dimension == vector_image_dimension
            and cast.dtype == t[1]
        )

# Test .astype for conversion between vector-like pixel types.
components = 3
numpyImage = np.random.randint(0, 256, (12, 8, components)).astype(np.uint8)
input_image = itk.image_from_array(numpyImage, is_vector=True)
if type(input_image) == itk.Image[itk.RGBPixel[itk.UC], 2] and hasattr(
    itk.CastImageFilter, "IRGBUC2IVF32"
):
    output_pixel_type = itk.Vector[itk.F, components]
    output_image = input_image.astype(output_pixel_type)
    assert type(output_image) == itk.Image[output_pixel_type, 2]

if "(<itkCType unsigned char>, 4)" in itk.Image.GetTypesAsList():
    arr = np.random.randint(0, 255, size=(3, 4, 5, 6), dtype=np.uint8)
    image = itk.image_view_from_array(arr)
    arr_back = itk.array_view_from_image(image)
    assert np.allclose(arr, arr_back)
    image = itk.image_from_array(arr)
    arr_back = itk.array_from_image(image)
    assert np.allclose(arr, arr_back)


# vtk conversion
try:
    import vtk

    print("Testing vtk conversion")
    image = itk.image_from_array(np.random.rand(2, 3, 4))
    z_rot = np.asarray([[0, 1, 0], [-1, 0, 0], [0, 0, 1]], dtype=np.float64)
    z_rot_itk = itk.matrix_from_array(z_rot)
    image.SetDirection(z_rot_itk)

    vtk_image = itk.vtk_image_from_image(image)
    image_round = itk.image_from_vtk_image(vtk_image)
    assert np.array_equal(itk.origin(image), itk.origin(image_round))
    assert np.array_equal(itk.spacing(image), itk.spacing(image_round))
    assert np.array_equal(itk.size(image), itk.size(image_round))
    assert np.array_equal(
        itk.array_view_from_image(image), itk.array_view_from_image(image_round)
    )
    if vtk.vtkVersion.GetVTKMajorVersion() >= 9:
        z_rot_round = itk.array_from_matrix(image_round.GetDirection())
        assert np.array_equal(z_rot, z_rot_round)
    else:
        print("VTK version <9. Direction unsupported.")

    image = itk.image_from_array(
        np.random.rand(5, 4, 2).astype(np.float32), is_vector=True
    )
    z_rot = np.asarray([[0, 1], [-1, 0]], dtype=np.float64)
    z_rot_itk = itk.matrix_from_array(z_rot)
    image.SetDirection(z_rot_itk)

    vtk_image = itk.vtk_image_from_image(image)
    image_round = itk.image_from_vtk_image(vtk_image)
    assert np.array_equal(itk.origin(image), itk.origin(image_round))
    assert np.array_equal(itk.spacing(image), itk.spacing(image_round))
    assert np.array_equal(itk.size(image), itk.size(image_round))
    assert np.array_equal(
        itk.array_view_from_image(image), itk.array_view_from_image(image_round)
    )
    if vtk.vtkVersion.GetVTKMajorVersion() >= 9:
        z_rot_round = itk.array_from_matrix(image_round.GetDirection())
        assert np.array_equal(z_rot, z_rot_round)

except ImportError:
    print("vtk not imported. Skipping vtk conversion tests")
    pass
