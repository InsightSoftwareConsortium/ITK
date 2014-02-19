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
from sys import argv
itk.auto_progress(2)

# define a custom templated pipeline


class LabelDilateImageFilter(itk.pipeline):

    def __init__(self, *args, **kargs):
        # call the constructor of the superclass but without args and kargs,
        # because the attributes are not all already there!
        # Set/GetRadius() is created in the constructor for example, with the
        # expose() method
        itk.pipeline.__init__(self)

        # get the template parameters
        template_parameters = kargs["template_parameters"]
        # check the template parameters validity. Not really useful in that
        # case, because we do the same here, but a good habit
        LabelDilateImageFilter.check_template_parameters(template_parameters)

        # and store them in an easier way
        ImageType, DistanceMapType = template_parameters

        # build the minipipeline
        self.connect(
            itk.DanielssonDistanceMapImageFilter[
                ImageType,
                DistanceMapType].New(
                UseImageSpacing=True,
                SquaredDistance=False))
        self.connect(
            itk.BinaryThresholdImageFilter[DistanceMapType,
                                           ImageType].New())
        self.expose("UpperThreshold", "Radius")
        self.append(
            itk.MaskImageFilter[ImageType,
                                ImageType,
                                ImageType].New(self.filters[0].GetVoronoiMap(),
                                               Input2=self.filters[1]))

        # now we can parse the inputs
        itk.set_inputs(self, args, kargs)

    def check_template_parameters(template_parameters):
        ImageType, DistanceMapType = template_parameters
        itk.DanielssonDistanceMapImageFilter[ImageType, DistanceMapType]
        itk.BinaryThresholdImageFilter[DistanceMapType, ImageType]
        itk.CastImageFilter[DistanceMapType, ImageType]
        itk.MaskImageFilter[ImageType, ImageType, ImageType]
    check_template_parameters = staticmethod(check_template_parameters)

LabelDilateImageFilter = itk.templated_class(LabelDilateImageFilter)


# and use it
dim = 2
IType = itk.Image[itk.UC, dim]
OIType = itk.Image[itk.UC, dim]
DIType = itk.Image[itk.F, dim]

reader = itk.ImageFileReader[IType].New(FileName=argv[1])
val = argv[3]
dilate = LabelDilateImageFilter[IType, DIType].New(reader, Radius=eval(val))
writer = itk.ImageFileWriter[OIType].New(dilate, FileName=argv[2])

writer.Update()
