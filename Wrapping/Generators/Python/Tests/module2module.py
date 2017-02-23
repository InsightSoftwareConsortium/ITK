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

from __future__ import print_function

import itk
import sys

from itk import ITKCommon
from itk import ITKBinaryMathematicalMorphology
from itk import ITKImageStatistics
from itk import ITKSmoothing
from itk import ITKDistanceMap
from itk import ITKImageIntensity
from itk import ITKThresholding
from itk import ITKImageGrid

inputImage = sys.argv[1]
radiusValue = int(sys.argv[2])

PixelType = itk.UC
Dimension = 2
ImageType = itk.Image[PixelType, Dimension]

StructuringElementType = itk.FlatStructuringElement[Dimension]
radius = (radiusValue, radiusValue)
structuringElement = StructuringElementType.Ball(radius)

# create the reader
reader = itk.ImageFileReader[ImageType].New(FileName=inputImage)

sources = []
image = ITKCommon.Image[PixelType, Dimension].New()
region = itk.ImageRegion.x2()
region.SetSize((10, 10))
image.SetRegions(region)
image.Allocate()

sources.append(("ITKCommon", image))

sources.append(("ITKIOImageBase", reader.GetOutput()))

OtsuType = ITKThresholding.OtsuThresholdImageFilter[ImageType, ImageType]
otsu = OtsuType.New(reader)
sources.append(("ITKThresholding", otsu.GetOutput()))

flip = ITKImageGrid.FlipImageFilter[ImageType].New(reader)
sources.append(("ITKImageGrid", flip.GetOutput()))

absFilter = ITKImageIntensity.AbsImageFilter[ImageType, ImageType].New(reader)
sources.append(("ITKImageIntensity", absFilter.GetOutput()))
absFilter.InPlaceOff()

BinaryDilateType = ITKBinaryMathematicalMorphology.BinaryDilateImageFilter[
    ImageType, ImageType, StructuringElementType]
binaryDilateFilter = BinaryDilateType.New()
binaryDilateFilter.SetInput(reader.GetOutput())
binaryDilateFilter.SetKernel(structuringElement)

output = binaryDilateFilter.GetOutput()
sources.append(("ITKBinaryMathematicalMorphology", output))

minmax = ITKImageStatistics.MinimumMaximumImageFilter[ImageType].New(reader)
sources.append(("ITKImageStatistics", minmax.GetOutput()))

median = ITKSmoothing.MedianImageFilter[ImageType, ImageType].New(reader)
sources.append(("ITKSmoothing", median.GetOutput()))

distance = ITKDistanceMap.DanielssonDistanceMapImageFilter[
    ImageType, ImageType].New(reader)
sources.append(("ITKDistanceMap", distance.GetOutput()))


dests = []

dotsu = OtsuType.New()
dests.append(("ITKThresholding", dotsu))

dflip = ITKImageGrid.FlipImageFilter[ImageType].New()
dests.append(("ITKImageGrid", dflip))

dabs = ITKImageIntensity.AbsImageFilter[ImageType, ImageType].New()
dests.append(("ITKImageIntensity", dabs))

dbinaryDilateFilter = BinaryDilateType.New()
dbinaryDilateFilter.SetKernel(structuringElement)
dests.append(("ITKBinaryMathematicalMorphology", dbinaryDilateFilter))

dminmax = ITKImageStatistics.MinimumMaximumImageFilter[ImageType].New()
dests.append(("ITKImageStatistics", dminmax))

dmedian = ITKSmoothing.MedianImageFilter[ImageType, ImageType].New()
dests.append(("ITKSmoothing", dmedian))

DistanceMapType = ITKDistanceMap.DanielssonDistanceMapImageFilter[ImageType,
                                                                  ImageType]
ddistance = DistanceMapType.New()
dests.append(("ITKDistanceMap", ddistance))


nb = 0
failList = []
for sourceName, source in sources:
    for destinationName, destination in dests:
        nb += 1
        destination.SetInput(source)
        try:
            destination.UpdateLargestPossibleRegion()
            print("%s -> %s pass" % (sourceName, destinationName))
        except RuntimeError as e:
            print("%s -> %s fail" % (sourceName, destinationName))
            print(e)
            failList.append((sourceName, destinationName))


print()
print("%i tests succeed" % (nb - len(failList)))
print("%i tests failed" % len(failList))

sys.exit(len(failList))
