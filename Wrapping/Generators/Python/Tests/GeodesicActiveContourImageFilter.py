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

# GeodesicActiveContourImageFilter.py
# Translated by Charl P. Botha <http://cpbotha.net/> from the cxx original.
# $Id: GeodesicActiveContourImageFilter.py,v 1.1 2006/09/06 20:58:42 glehmann
# Exp $

# example runs:
# ------------
# 1. Left ventricle:
# python GeodesicActiveContourImageFilter.py \
# ../Data/BrainProtonDensitySlice.png lventricle.png \
# 81 114 5 1 -0.5 3 2
#
# 2. White matter:
# python GeodesicActiveContourImageFilter.py \
# ../Data/BrainProtonDensitySlice.png wmatter.png \
# 56 92 5 1 -0.3 2 10
#
# See the ITK Software Guide, section 9.3.3 "Geodesic Active Contours
# Segmentation" as well as the CXX example for more comments.

from __future__ import print_function

import itk
from sys import argv, stderr
import os
itk.auto_progress(2)


def main():
    if len(argv) < 10:
        errMsg = "Missing parameters\n" \
                 "Usage: %s\n" % (argv[0],) + \
                 " inputImage  outputImage\n" \
                 " seedX seedY InitialDistance\n" \
                 " Sigma SigmoidAlpha SigmoidBeta\n" \
                 " PropagationScaling\n"

        print(errMsg, file=stderr)
        return

    # We're going to build the following pipelines:
    # 1. reader -> smoothing -> gradientMagnitude -> sigmoid -> FI
    # 2. fastMarching -> geodesicActiveContour(FI) -> thresholder -> writer
    # The output of pipeline 1 is a feature image that is used by the
    # geodesicActiveContour object.  Also see figure 9.18 in the ITK
    # Software Guide.

    # we wan't to know what is happening
    # itk.auto_progress(True)

    InternalPixelType = itk.F
    Dimension = 2
    InternalImageType = itk.Image[InternalPixelType, Dimension]

    OutputPixelType = itk.UC
    OutputImageType = itk.Image[OutputPixelType, Dimension]

    reader = itk.ImageFileReader[InternalImageType].New(FileName=argv[1])
    # needed to give the size to the fastmarching filter
    reader.Update()

    outputDirectory = os.path.dirname(argv[2])

    smoothing = itk.CurvatureAnisotropicDiffusionImageFilter[
        InternalImageType,
        InternalImageType].New(
        reader,
        TimeStep=0.125,
        NumberOfIterations=5,
        ConductanceParameter=9.0)

    gradientMagnitude = itk.GradientMagnitudeRecursiveGaussianImageFilter[
        InternalImageType,
        InternalImageType].New(
        smoothing,
        Sigma=float(argv[6]))

    sigmoid = itk.SigmoidImageFilter[InternalImageType, InternalImageType].New(
        gradientMagnitude,
        OutputMinimum=0.0,
        OutputMaximum=1.1,
        Alpha=float(argv[7]),
        Beta=float(argv[8]))

    seedPosition = itk.Index[2]()
    seedPosition.SetElement(0, int(argv[3]))
    seedPosition.SetElement(1, int(argv[4]))

    node = itk.LevelSetNode[InternalPixelType, Dimension]()
    node.SetValue(-float(argv[5]))
    node.SetIndex(seedPosition)

    seeds = itk.VectorContainer[
        itk.UI, itk.LevelSetNode[InternalPixelType, Dimension]].New()
    seeds.Initialize()
    seeds.InsertElement(0, node)

    fastMarching = itk.FastMarchingImageFilter[
        InternalImageType,
        InternalImageType].New(
        sigmoid,
        TrialPoints=seeds,
        SpeedConstant=1.0,
        OutputSize=reader.GetOutput().GetBufferedRegion().GetSize())

    geodesicActiveContour = itk.GeodesicActiveContourLevelSetImageFilter[
        InternalImageType,
        InternalImageType,
        InternalPixelType].New(
        fastMarching,
        # it is required to use the explicitly the FeatureImage
        # - itk segfault without that :-(
        FeatureImage=sigmoid.GetOutput(),
        PropagationScaling=float(argv[9]),
        CurvatureScaling=1.0,
        AdvectionScaling=1.0,
        MaximumRMSError=0.02,
        NumberOfIterations=800)

    thresholder = itk.BinaryThresholdImageFilter[
        InternalImageType,
        OutputImageType].New(
        geodesicActiveContour,
        LowerThreshold=-1000,
        UpperThreshold=0,
        OutsideValue=0,
        InsideValue=255)

    writer = itk.ImageFileWriter[OutputImageType].New(
        thresholder,
        FileName=argv[2])

    def rescaleAndWrite(filter, fileName):
        caster = itk.RescaleIntensityImageFilter[
            InternalImageType,
            OutputImageType].New(
            filter,
            OutputMinimum=0,
            OutputMaximum=255)
        itk.write(caster, os.path.join(outputDirectory, fileName))

    rescaleAndWrite(smoothing, "GeodesicActiveContourImageFilterOutput1.png")
    rescaleAndWrite(
        gradientMagnitude,
        "GeodesicActiveContourImageFilterOutput2.png")
    rescaleAndWrite(sigmoid, "GeodesicActiveContourImageFilterOutput3.png")
    rescaleAndWrite(
        fastMarching,
        "GeodesicActiveContourImageFilterOutput4.png")

    writer.Update()

    print("")
    print(
        "Max. no. iterations: %d" %
        (geodesicActiveContour.GetNumberOfIterations()))
    print(
        "Max. RMS error: %.3f" %
        (geodesicActiveContour.GetMaximumRMSError()))
    print("")
    print(
        "No. elapsed iterations: %d"
        % (geodesicActiveContour.GetElapsedIterations()))
    print("RMS change: %.3f" % (geodesicActiveContour.GetRMSChange()))

    itk.write(fastMarching, os.path.join(outputDirectory,
        "GeodesicActiveContourImageFilterOutput4.mha"))
    itk.write(sigmoid, os.path.join(outputDirectory,
        "GeodesicActiveContourImageFilterOutput3.mha"))
    itk.write(gradientMagnitude, os.path.join(outputDirectory,
        "GeodesicActiveContourImageFilterOutput2.mha"))


if __name__ == "__main__":
    main()
