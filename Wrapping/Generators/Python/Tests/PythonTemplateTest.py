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

import itk
import tempfile
import os

dim = 2
PixelType = itk.UC

# check the repr string
assert "<itkTemplate itk::Image>" == repr(itk.Image)

# template should work with CType instance and with numbers
ImageType = itk.Image[PixelType, dim]

# template should return the same class with a class as parameter
# or with an object of this class, and should also be the same
# with the attribute

# create instances of image for the next tests
im = ImageType.New()
im2 = ImageType.New()

readerType = itk.ImageFileReader[ImageType]
readerType2 = itk.ImageFileReader[im]
readerType3 = itk.ImageFileReader.IUC2

assert readerType == readerType2 == readerType3

# we should be able to get the template and its parameters from the class
(tpl, parameters) = itk.template(ImageType)
assert tpl == itk.Image
assert parameters == (PixelType, dim)

# the template must raise a KeyError exception if the template parameter
# is unknown
try:
    itk.ImageFileReader['unknown parameter']
    raise Exception('no exception sent for unknown parameter')
except KeyError:
    pass

# TODO: test the rest of the dict interface
# TODO: test __eq__, __ne__ and __hash__

# something else ?


# now test the New method
# without parameter
reader = readerType.New()
reader2 = readerType.New()

# with an attribute parameter
reader = readerType.New(FileName='test.png')
assert reader.GetFileName() == 'test.png'

# wwith a wrong attribute name
try:
    reader = readerType.New(WrongName='test.png')
    raise Exception('no exception sent for wrong attribute name')
except AttributeError:
    pass

# wwith a wrong attribute type
try:
    reader = readerType.New(FileName=1)
    raise Exception('no exception sent for wrong attribute type')
except:
    pass

# pass filter as argument for input
# to a filter with SetInput method
median = itk.MedianImageFilter[ImageType, ImageType].New(reader)
assert reader.GetOutput() == median.GetInput()

# filter type determined by the input passed as an arg
median_args = itk.MedianImageFilter.New(reader.GetOutput())
assert itk.class_(median) == itk.class_(median_args)

# filter type determined by the input passed as a primary method input
median_kwarg = itk.MedianImageFilter.New(Input=reader.GetOutput())
assert itk.class_(median) == itk.class_(median_kwarg)

# to a filter with a SetImage method
calculator = itk.MinimumMaximumImageCalculator[ImageType].New(reader)
# not GetImage() method here to verify it's the right image

# to a filter with several inputs
sub = itk.SubtractImageFilter[ImageType, ImageType, ImageType].New(reader, reader2)
assert reader.GetOutput() == sub.GetInput(0)
assert reader2.GetOutput() == sub.GetInput(1)


# pass image as argument for input
# to a filter with SetInput method
median = itk.MedianImageFilter[ImageType, ImageType].New(im)
assert im == median.GetInput()

# to a filter with a SetImage method
calculator = itk.MinimumMaximumImageCalculator[ImageType].New(im)
# not GetImage() method here to verify it's the right image

# to a filter with several inputs
sub = itk.SubtractImageFilter[ImageType, ImageType, ImageType].New(im, im2)
assert im == sub.GetInput(0)
assert im2 == sub.GetInput(1)


# pass invalid input
try:
    itk.MedianImageFilter[ImageType, ImageType].New(1)
    raise Exception('no exception sent for wrong input type')
except:
    pass

try:
    itk.SubtractImageFilter[ImageType, ImageType, ImageType].New(im, "wrong")
    raise Exception('no exception sent for wrong 2nd input type')
except TypeError:
    pass


# pass both input and attribute
recons = itk.ReconstructionByDilationImageFilter[
    ImageType, ImageType].New(reader.GetOutput(), im, FullyConnected=True)
assert reader.GetOutput() == recons.GetInput(0)
assert im == recons.GetInput(1)
assert recons.GetFullyConnected()


# pass input to object which do not take one
try:
    ImageType.New(im)
    raise Exception('no exception sent for object without input')
except AttributeError:
    pass

# pass no attribute to reader and see if it automatically detects input image type.
# Since we cannot easily know which type of images are supported in python, we generate
# a lot of different types and try to create images using these types, create empty images
# that we save, and then try to read them to test the reader.
# We ignore the exception raised for creating types of pixels and images and from trying to
# write the images on the hard disk the first time. We only care about exception raised from
# the reader.

def pixel_type_from_IO(pixel, component, dimension):
    import itk
    if pixel == 'scalar':
        PixelType = component
    elif pixel == 'rgb':
        PixelType = itk.RGBPixel[component]
    elif pixel == 'rgba':
        PixelType = itk.RGBAPixel[component]
    elif pixel == 'offset':
        PixelType = itk.Offset[dimension]
    elif pixel == 'vector':
        PixelType = itk.Vector[component, dimension]
    elif pixel == 'point':
        PixelType = itk.Point[component, dimension]
    elif pixel == 'covariant_vector':
        PixelType = itk.CovariantVector[component, dimension]
    elif pixel == 'symmetric_second_rank_tensor':
        PixelType = itk.SymmetricSecondRankTensor[component, dimension]
    elif pixel == 'diffusion_tensor_3D':
        PixelType = itk.DiffusionTensor3D[component]
    elif pixel == 'complex':
        PixelType = itk.complex[component]
    elif pixel == 'fixed_array':
        PixelType = itk.FixedArray[component, dimension]
    elif pixel == 'matrix':
        PixelType = itk.Matrix[component, dimension, dimension]
    else:
        raise RuntimeError("Unknown pixel type %s." % pixel)
    return PixelType

dimensions = [2, 3]
component_type_dic= {"float":itk.F, "double":itk.D,
        "unsigned_char":itk.UC, "unsigned_short":itk.US, "unsigned_int":itk.UI,
        "unsigned_long":itk.UL, "char":itk.SC, "short":itk.SS,
        "int":itk.SI, "long":itk.SL, "bool":itk.B}
pixel_types = ['scalar', 'rgb', 'rgba', 'offset', 'vector', 'point',
               'covariant_vector', 'symmetric_second_rank_tensor',
               'diffusion_tensor_3D', 'complex', 'fixed_array', 'matrix']
dir_name=tempfile.mkdtemp()
pixel_type_error = []
image_error = []
other_error = []
writer_error = []
reader_error = []
pixel_fill_error = []
for dim in dimensions:
    for p in pixel_types:
        for c, cv in component_type_dic.items():
            try:
                PixelType = pixel_type_from_IO(p, cv, dim)
            except Exception as ex:
                pixel_type_error.append(ex)
                continue
            try:
                ImageType=itk.Image[PixelType, dim]
                im=ImageType.New()
            except Exception as ex:
                image_error.append(ex)
                continue
            try:
                zero_value = itk.NumericTraits[PixelType].ZeroValue()
            except:
                try:
                    zero_value = PixelType()
                    zero_value.Fill(0)
                except:
                    try:
                        zero_value = PixelType(0)
                    except Exception as ex:
                        pixel_fill_error.append(ex)
            try:
                im.SetRegions(5)
                im.Allocate()
                im.FillBuffer( zero_value )
                filename = os.path.join(dir_name, "_".join([p,c,str(dim)]) + ".nrrd")
            except Exception as ex:
                other_error.append(ex)
                continue
            try:
                writer = itk.ImageFileWriter.New(Input=im, FileName=filename)
                writer.Update()
            except Exception as ex:
                writer_error.append(str(ex) + " - " + str([p, c, dim]))
                continue
            try:
                reader = itk.ImageFileReader.New(FileName=filename)
                reader.Update()
                print("Dimension: %i ; Pixel Type: %s ; Component Type: %s - OK" % (dim, p, c))
            except Exception as ex:
                reader_error.append(ex)
                continue
if reader_error or other_error:
    print("PixelType error: %s" % pixel_type_error)
    print("Image error: %s" % image_error)
    print("Other error: %s" % other_error)
    print("Writer error: %s" % writer_error)
    print("Pixel Fill error: %s" % pixel_fill_error)
    print("Reader error: %s" % reader_error)
    raise AssertionError()

# Test that ImageFileWriter can be called with filter given as input
# with 'Input' keyword.
reader=itk.ImageFileReader[itk.Image[itk.F,3]].New()
try:
    writer = itk.ImageFileWriter.New(Input=reader, FileName=filename)
except TypeError:
    raise Exception("Writer doesn't accept filter with 'Input' keyword")

# TODO: test auto_progress
# but how ?

# something else ?
