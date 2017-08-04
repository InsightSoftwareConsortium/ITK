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

# test the force load function
itk.force_load()

fileName = sys.argv[1]

PixelType = itk.UC
dim = 2
ImageType = itk.Image[PixelType, dim]
ReaderType = itk.ImageFileReader[ImageType]
reader = ReaderType.New(FileName=fileName)


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
itk.write(reader, sys.argv[2])
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

# pipeline, auto_pipeline and templated class are tested in other files

# BridgeNumPy
try:
    # Images
    import numpy
    image = itk.imread(fileName)
    arr = itk.GetArrayFromImage(image)
    arr.fill(1)
    assert numpy.any(arr != itk.GetArrayFromImage(image))
    view = itk.GetArrayViewFromImage(image)
    view.fill(1)
    assert numpy.all(view == itk.GetArrayFromImage(image))
    image = itk.GetImageFromArray(arr)
    image.FillBuffer(2)
    assert numpy.any(arr != itk.GetArrayFromImage(image))
    image = itk.GetImageViewFromArray(arr)
    image.FillBuffer(2)
    assert numpy.all(arr == itk.GetArrayFromImage(image))
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
    arr = numpy.zeros([3,3])
    m_vnl = itk.GetVnlMatrixFromArray(arr)
    assert m_vnl(0,0) == 0
    m_vnl.put(0,0,3)
    assert m_vnl(0,0) == 3
    assert arr[0,0] == 0

except ImportError:
    print("NumPy not imported. Skipping BridgeNumPy tests")
    # Numpy is not available, do not run the Bridge NumPy tests
    pass
