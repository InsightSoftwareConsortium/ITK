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

# def custom_callback(name):
#     print "loading %s submodule..." % name
# import itkConfig
# itkConfig.ImportCallback = custom_callback

import itk
import sys

import ITKCommon
import ITKBinaryMathematicalMorphology
import ITKImageStatistics
import ITKSmoothing
import ITKDistanceMap
import ITKImageIntensity
import ITKIOImageBase
import ITKThresholding
import ITKImageGrid

inputImage = sys.argv[1]
radiusValue = int(sys.argv[2])

PType = itk.UC
Dimension = 2
IType = itk.Image[PType, Dimension]

StructuringElementType = itk.FlatStructuringElement[Dimension]
structuringElement = StructuringElementType.Ball(radiusValue)

# create the reader
reader = itk.ImageFileReader[IType].New(FileName=inputImage)

sources = []
image = ITKCommon.Image[PType, Dimension].New()
r = itk.ImageRegion._2()
r.SetSize((10, 10))
image.SetRegions(r)
image.Allocate()

sources.append(("ITKCommon", image))

sources.append(("ITKIOImageBase", reader.GetOutput()))

otsu = ITKThresholding.OtsuThresholdImageFilter[IType, IType].New(reader)
sources.append(("ITKThresholding", otsu.GetOutput()))

flip = ITKImageGrid.FlipImageFilter[IType].New(reader)
sources.append(("ITKImageGrid", flip.GetOutput()))

abs = ITKImageIntensity.AbsImageFilter[IType, IType].New(reader)
sources.append(("ITKImageIntensity", abs.GetOutput()))

binarydilateType = ITKBinaryMathematicalMorphology.BinaryDilateImageFilter[
    IType, IType, StructuringElementType]
binarydilateFilter = binarydilateType.New()
binarydilateFilter.SetInput(reader.GetOutput())
binarydilateFilter.SetKernel(structuringElement)

output = binarydilateFilter.GetOutput()
sources.append(("ITKBinaryMathematicalMorphology", output))

minmax = ITKImageStatistics.MinimumMaximumImageFilter[IType].New(reader)
sources.append(("ITKImageStatistics", minmax.GetOutput()))

median = ITKSmoothing.MedianImageFilter[IType, IType].New(reader)
sources.append(("ITKSmoothing", median.GetOutput()))

distance = ITKDistanceMap.DanielssonDistanceMapImageFilter[
    IType, IType].New(reader)
sources.append(("ITKDistanceMap", distance.GetOutput()))

# sobel = EdgesAndContours.SobelEdgeDetectionImageFilter[IType, IType].New
# (reader)
# sources.append(("EdgesAndContours", sobel.GetOutput()))

# laplacian = Filtering.LaplacianImageFilter[IType, IType].New(reader)
# sources.append(("Filtering", laplacian.GetOutput()))


dests = []

dotsu = ITKThresholding.OtsuThresholdImageFilter[IType, IType].New(reader)
dests.append(("ITKThresholding", dotsu))

dflip = ITKImageGrid.FlipImageFilter[IType].New()
dests.append(("ITKImageGrid", dflip))

dabs = ITKImageIntensity.AbsImageFilter[IType, IType].New()
dests.append(("ITKImageIntensity", dabs))

binarydilateType = ITKBinaryMathematicalMorphology.BinaryDilateImageFilter[
    IType, IType, StructuringElementType]
binarydilateFilter = binarydilateType.New()
binarydilateFilter.SetKernel(structuringElement)
dests.append(("ITKBinaryMathematicalMorphology", binarydilateFilter))

dminmax = ITKImageStatistics.MinimumMaximumImageFilter[IType].New()
dests.append(("ITKImageStatistics", dminmax))

dmedian = ITKSmoothing.MedianImageFilter[IType, IType].New()
dests.append(("ITKSmoothing", dmedian))

ddistance = ITKDistanceMap.DanielssonDistanceMapImageFilter[IType, IType].New()
dests.append(("ITKDistanceMap", ddistance))

# dsobel = EdgesAndContours.SobelEdgeDetectionImageFilter[IType, IType].New()
# dests.append(("EdgesAndContours", dsobel))

# dlaplacian = Filtering.LaplacianImageFilter[IType, IType].New()
# dests.append(("Filtering", dlaplacian))

writer = ITKIOImageBase.ImageFileWriter[IType].New(FileName='out.png')
dests.append(("ITKIOImageBase", writer))


nb = 0
failList = []
for sname, s in sources:
    for dname, d in dests:
        nb += 1
        d.SetInput(s)
        try:
            d.Update()
            print "%s -> %s pass" % (sname, dname)
        except RuntimeError as e:
            print "%s -> %s fail" % (sname, dname)
            failList.append((sname, dname))


print
print "%i tests succeed" % (nb - len(failList))
print "%i tests failed" % len(failList)

sys.exit(len(failList))
