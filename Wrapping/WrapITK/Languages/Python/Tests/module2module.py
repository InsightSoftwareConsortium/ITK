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
#   print "loading %s submodule..." % name
# import itkConfig
# itkConfig.ImportCallback = custom_callback

import itk, sys

import ITKCommon
import ITKMathematicalMorphology
import ITKImageStatistics
import ITKSmoothing
import ITKDistanceMap
import ITKImageIntensity
import ITKIOBase
import ITKThresholding
import ITKImageGrid

PType = itk.UC
dim = 2
IType = itk.Image[PType, dim]

kernel = itk.strel(2, 1)

# create the reader
reader = itk.ImageFileReader[IType].New(FileName=sys.argv[1])

sources = []
image = ITKCommon.Image[PType, dim].New()
r = itk.ImageRegion._2()
r.SetSize((10, 10))
image.SetRegions(r)
image.Allocate()

sources.append(("ITKCommon", image))

sources.append(("ITKIOBase", reader.GetOutput()))

otsu = ITKThresholding.OtsuThresholdImageFilter[IType, IType].New(reader)
sources.append(("ITKThresholding", otsu.GetOutput()))

flip = ITKImageGrid.FlipImageFilter[IType].New(reader)
sources.append(("ITKImageGrid", flip.GetOutput()))

abs = ITKImageIntensity.AbsImageFilter[IType, IType].New(reader)
sources.append(("ITKImageIntensity", abs.GetOutput()))

bdilate = ITKMathematicalMorphology.BinaryDilateImageFilter[IType, IType, kernel].New(reader, Kernel=kernel)
sources.append(("ITKMathematicalMorphology", bdilate.GetOutput()))

minmax = ITKImageStatistics.MinimumMaximumImageFilter[IType].New(reader)
sources.append(("ITKImageStatistics", minmax.GetOutput()))

median = ITKSmoothing.MedianImageFilter[IType, IType].New(reader)
sources.append(("ITKSmoothing", median.GetOutput()))

distance = ITKDistanceMap.DanielssonDistanceMapImageFilter[IType, IType].New(reader)
sources.append(("ITKDistanceMap", distance.GetOutput()))

# sobel = EdgesAndContours.SobelEdgeDetectionImageFilter[IType, IType].New(reader)
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

dbdilate = ITKMathematicalMorphology.BinaryDilateImageFilter[IType, IType, kernel].New(Kernel=kernel)
dests.append(("ITKMathematicalMorphology", dbdilate))

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

writer = ITKIOBase.ImageFileWriter[IType].New(FileName='out.png')
dests.append(("ITKIOBase", writer))


nb = 0
failList = []
for sname, s in sources:
  for dname, d in dests:
    nb += 1
    d.SetInput( s )
    try:
      d.Update()
      print "%s -> %s pass" % (sname, dname)
    except RuntimeError, e:
      print "%s -> %s fail" % (sname, dname)
      failList.append((sname, dname))


print
print "%i tests succeed" % (nb - len(failList))
print "%i tests failed" % len(failList)

sys.exit(len(failList))
