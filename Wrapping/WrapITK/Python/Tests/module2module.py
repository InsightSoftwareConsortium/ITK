#!/usr/bin/env python

# def custom_callback(name):
#   print "loading %s submodule..." % name
# import itkConfig
# itkConfig.ImportCallback = custom_callback

import itk, sys

import Base
import BaseNumerics
import BaseSpatialObject
import BaseTransforms
import BinaryMorphology
import Calculators
import Compose
import DeformableTransforms
import Denoising
import DistanceMap
import EdgesAndContours
import FFT
import Filtering
import IntensityFilters
import Interpolators
import IO
# import Iterators
import LevelSet
import Morphology
# import Patented
import PixelMath
import Registration
import Resize
import SegmentationAndThreshold
import SegmentationValidation
import SimpleFilters
import UnaryPixelMath
import VXLNumerics


PType = itk.US
dim = 2
IType = itk.Image[PType, dim]

kernel = itk.strel(2, 1)

# create the reader
reader = itk.ImageFileReader[IType].New(FileName=sys.argv[1])

sources = []
image = Base.Image[PType, dim].New()
r = itk.ImageRegion._2()
r.SetSize((10, 10))
image.SetRegions(r)
image.Allocate()



# BaseNumerics
# BaseSpatialObject
# BaseTransforms
# Compose
# DeformableTransforms
# FFT
# Interpolators
# Iterators
# LevelSet
# Patented
# PixelMath
# Registration
# SegmentationValidation
# VXLNumerics
# EdgesAndContours
# Filtering


sources.append(("Base", image))

sources.append(("IO", reader.GetOutput()))

otsu = SegmentationAndThreshold.OtsuThresholdImageFilter[IType, IType].New(reader)
sources.append(("SegmentationAndThreshold", otsu.GetOutput()))

flip = SimpleFilters.FlipImageFilter[IType].New(reader)
sources.append(("SimpleFilters", flip.GetOutput()))

abs = UnaryPixelMath.AbsImageFilter[IType, IType].New(reader)
sources.append(("UnaryPixelMath", abs.GetOutput()))

bdilate = BinaryMorphology.BinaryDilateImageFilter[IType, IType, kernel].New(reader, Kernel=kernel)
sources.append(("BinaryMorphology", bdilate.GetOutput()))

minmax = Calculators.MinimumMaximumImageFilter[IType].New(reader)
sources.append(("Calculators", minmax.GetOutput()))

median = Denoising.MedianImageFilter[IType, IType].New(reader)
sources.append(("Denoising", median.GetOutput()))

distance = DistanceMap.DanielssonDistanceMapImageFilter[IType, IType].New(reader)
sources.append(("DistanceMap", distance.GetOutput()))

# sobel = EdgesAndContours.SobelEdgeDetectionImageFilter[IType, IType].New(reader)
# sources.append(("EdgesAndContours", sobel.GetOutput()))

# laplacian = Filtering.LaplacianImageFilter[IType, IType].New(reader)
# sources.append(("Filtering", laplacian.GetOutput()))

invert = IntensityFilters.InvertIntensityImageFilter[IType, IType].New(reader)
sources.append(("IntensityFilters", invert.GetOutput()))

hmax = Morphology.HMaximaImageFilter[IType, IType].New(reader)
sources.append(("Morphology", hmax.GetOutput()))

crop = Resize.CropImageFilter[IType, IType].New(reader)
sources.append(("Resize", crop.GetOutput()))






dests = []

dotsu = SegmentationAndThreshold.OtsuThresholdImageFilter[IType, IType].New(reader)
dests.append(("SegmentationAndThreshold", dotsu))

dflip = SimpleFilters.FlipImageFilter[IType].New()
dests.append(("SimpleFilters", dflip))

dabs = UnaryPixelMath.AbsImageFilter[IType, IType].New()
dests.append(("UnaryPixelMath", dabs))

dbdilate = BinaryMorphology.BinaryDilateImageFilter[IType, IType, kernel].New(Kernel=kernel)
dests.append(("BinaryMorphology", dbdilate))

dminmax = Calculators.MinimumMaximumImageFilter[IType].New()
dests.append(("Calculators", dminmax))

dmedian = Denoising.MedianImageFilter[IType, IType].New()
dests.append(("Denoising", dmedian))

ddistance = DistanceMap.DanielssonDistanceMapImageFilter[IType, IType].New()
dests.append(("DistanceMap", ddistance))

# dsobel = EdgesAndContours.SobelEdgeDetectionImageFilter[IType, IType].New()
# dests.append(("EdgesAndContours", dsobel))

# dlaplacian = Filtering.LaplacianImageFilter[IType, IType].New()
# dests.append(("Filtering", dlaplacian))

dinvert = IntensityFilters.InvertIntensityImageFilter[IType, IType].New()
dests.append(("IntensityFilters", dinvert))

dhmax = Morphology.HMaximaImageFilter[IType, IType].New()
dests.append(("Morphology", dhmax))

dcrop = Resize.CropImageFilter[IType, IType].New()
dests.append(("Resize", dcrop))

writer = IO.ImageFileWriter[IType].New(FileName='out.png')
dests.append(("IO", writer))


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