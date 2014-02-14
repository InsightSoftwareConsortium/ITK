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

dim = 2
PType = itk.UC

# check the repr string
assert "<itkTemplate itk::Image>" == repr(itk.Image)

# template should work with CType instance and with numbers
IType = itk.Image[PType, dim]

# template should return the same class with a class as parameter
# or with an object of this class, and should also be the same
# with the attribute

# create instances of image for the next tests
im = IType.New()
im2 = IType.New()

readerType = itk.ImageFileReader[IType]
readerType2 = itk.ImageFileReader[im]
readerType3 = itk.ImageFileReader.IUC2

assert readerType == readerType2 == readerType3

# we should be able to get the template and its parameters from the class
(tpl, parameters) = itk.template(IType)
assert tpl == itk.Image
assert parameters == (PType, dim)

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
median = itk.MedianImageFilter[IType, IType].New(reader)
assert reader.GetOutput() == median.GetInput()

# to a filter with a SetImage method
calculator = itk.MinimumMaximumImageCalculator[IType].New(reader)
# not GetImage() method here to verify it's the right image

# to a filter with several inputs
sub = itk.SubtractImageFilter[IType, IType, IType].New(reader, reader2)
assert reader.GetOutput() == sub.GetInput(0)
assert reader2.GetOutput() == sub.GetInput(1)


# pass image as argument for input
# to a filter with SetInput method
median = itk.MedianImageFilter[IType, IType].New(im)
assert im == median.GetInput()

# to a filter with a SetImage method
calculator = itk.MinimumMaximumImageCalculator[IType].New(im)
# not GetImage() method here to verify it's the right image

# to a filter with several inputs
sub = itk.SubtractImageFilter[IType, IType, IType].New(im, im2)
assert im == sub.GetInput(0)
assert im2 == sub.GetInput(1)


# pass invalid input
try:
    itk.MedianImageFilter[IType, IType].New(1)
    raise Exception('no exception sent for wrong input type')
except:
    pass

try:
    itk.SubtractImageFilter[IType, IType, IType].New(im, "wrong")
    raise Exception('no exception sent for wrong 2nd input type')
except TypeError:
    pass


# pass both input and attribute
recons = itk.ReconstructionByDilationImageFilter[
    IType, IType].New(reader.GetOutput(), im, FullyConnected=True)
assert reader.GetOutput() == recons.GetInput(0)
assert im == recons.GetInput(1)
assert recons.GetFullyConnected()


# pass input to object which do not take one
try:
    IType.New(im)
    raise Exception('no exception sent for object without input')
except AttributeError:
    pass

# TODO: test auto_progress
# but how ?

# something else ?
