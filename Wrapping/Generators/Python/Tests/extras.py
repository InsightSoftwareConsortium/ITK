#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

# also test the import callback feature

from __future__ import print_function


def custom_callback(name, progress):
    if progress == 0:
        print("Loading %s..." % name, file=sys.stderr)
    if progress == 1:
        print("done", file=sys.stderr)
import itkConfig
itkConfig.ImportCallback = custom_callback

import itk
import sys
import os

# test the force load function
itk.force_load()

fileName = sys.argv[1]

PixelType = itk.UC
dim = 2
ImageType = itk.Image[PixelType, dim]
ReaderType = itk.ImageFileReader[ImageType]
reader = ReaderType.New(FileName=fileName)

# test snake_case keyword arguments
reader = ReaderType.New(file_name=fileName)

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
itk.imwrite(reader, sys.argv[2])
itk.imwrite(reader, sys.argv[2], True)

# test read
image=itk.imread(fileName)
assert type(image) == itk.Image[itk.RGBPixel[itk.UC],2]
image=itk.imread(fileName, itk.F)
assert type(image) == itk.Image[itk.F,2]

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
assert png_io.GetFileName() == ''
reader=itk.ImageFileReader.New(FileName=fileName, ImageIO=png_io)
reader.Update()
assert png_io.GetFileName() == fileName

# test reading image series
series_reader = itk.ImageSeriesReader.New(FileNames=[fileName,fileName])
series_reader.Update()
assert series_reader.GetOutput().GetImageDimension() == 3
assert series_reader.GetOutput().GetLargestPossibleRegion().GetSize()[2] == 2

# test reading image series and check that dimension is not increased if
# last dimension is 1.
image_series = itk.Image[itk.UC, 3].New()
image_series.SetRegions([10, 7, 1])
image_series.Allocate()
image_series.FillBuffer(0)
image_series3d_filename = os.path.join(
    sys.argv[3], "image_series_extras_py.mha")
itk.imwrite(image_series, image_series3d_filename)
series_reader = itk.ImageSeriesReader.New(
    FileNames=[image_series3d_filename, image_series3d_filename])
series_reader.Update()
assert series_reader.GetOutput().GetImageDimension() == 3

# test reading image series with itk.imread()
image_series = itk.imread([fileName, fileName])
assert image_series.GetImageDimension() == 3

# Numeric series filename generation without any integer index. It is
# only to produce an ITK object that users could set as an input to
# `itk.ImageSeriesReader.New()` or `itk.imread()` and test that it works.
numeric_series_filename = itk.NumericSeriesFileNames.New()
numeric_series_filename.SetStartIndex(0)
numeric_series_filename.SetEndIndex(3)
numeric_series_filename.SetIncrementIndex(1)
numeric_series_filename.SetSeriesFormat(fileName)
image_series = itk.imread(numeric_series_filename.GetFileNames())
number_of_files = len(numeric_series_filename.GetFileNames())
assert image_series.GetImageDimension() == 3
assert image_series.GetLargestPossibleRegion().GetSize()[2] == number_of_files

# test reading image series with `itk.imread()` and check that dimension is
# not increased if last dimension is 1.
image_series = itk.imread([image_series3d_filename, image_series3d_filename])
assert image_series.GetImageDimension() == 3

# pipeline, auto_pipeline and templated class are tested in other files

# BridgeNumPy
try:
    # Images
    import numpy as np
    image = itk.imread(fileName)
    arr = itk.GetArrayFromImage(image)
    arr.fill(1)
    assert np.any(arr != itk.GetArrayFromImage(image))
    arr = itk.array_from_image(image)
    arr.fill(1)
    assert np.any(arr != itk.GetArrayFromImage(image))
    view = itk.GetArrayViewFromImage(image)
    view.fill(1)
    assert np.all(view == itk.GetArrayFromImage(image))
    image = itk.GetImageFromArray(arr)
    image.FillBuffer(2)
    assert np.any(arr != itk.GetArrayFromImage(image))
    image = itk.GetImageViewFromArray(arr)
    image.FillBuffer(2)
    assert np.all(arr == itk.GetArrayFromImage(image))
    image = itk.GetImageFromArray(arr, is_vector=True)
    assert image.GetImageDimension() == 2
    image = itk.GetImageViewFromArray(arr, is_vector=True)
    assert image.GetImageDimension() == 2
    arr = np.array([[1,2,3],[4,5,6]]).astype(np.uint8)
    assert arr.shape[0] == 2
    assert arr.shape[1] == 3
    assert arr[1,1] == 5
    image = itk.GetImageFromArray(arr)
    arrKeepAxes = itk.GetArrayFromImage(image, keep_axes=True)
    assert arrKeepAxes.shape[0] == 3
    assert arrKeepAxes.shape[1] == 2
    assert arrKeepAxes[1,1] == 4
    arr = itk.GetArrayFromImage(image, keep_axes=False)
    assert arr.shape[0] == 2
    assert arr.shape[1] == 3
    assert arr[1,1] == 5
    arrKeepAxes = itk.GetArrayViewFromImage(image, keep_axes=True)
    assert arrKeepAxes.shape[0] == 3
    assert arrKeepAxes.shape[1] == 2
    assert arrKeepAxes[1,1] == 4
    arr = itk.GetArrayViewFromImage(image, keep_axes=False)
    assert arr.shape[0] == 2
    assert arr.shape[1] == 3
    assert arr[1,1] == 5
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
    m1 = itk.vnl_matrix.D(2,2)
    m1.fill(1)
    m_np = itk.GetArrayFromVnlMatrix(m1)
    assert m1.get(0,0) == m_np[0,0]
    m_np[0,0] = 0
    assert m1.get(0,0) != m_np[0,0]
    view = itk.GetArrayViewFromVnlMatrix(m1)
    assert m1.get(0,0) == view[0,0]
    view[0,0] = 0
    assert m1.get(0,0) == view[0,0]
    arr = np.zeros([3,3])
    m_vnl = itk.GetVnlMatrixFromArray(arr)
    assert m_vnl(0,0) == 0
    m_vnl.put(0,0,3)
    assert m_vnl(0,0) == 3
    assert arr[0,0] == 0

except ImportError:
    print("NumPy not imported. Skipping BridgeNumPy tests")
    # Numpy is not available, do not run the Bridge NumPy tests
    pass
