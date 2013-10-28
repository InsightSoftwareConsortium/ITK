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
# Id

# NOTE: This example won't work if your ITK is older that 2004-02-26

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

import InsightToolkit as itk
import sys

def main():
    if len(sys.argv) < 10:
        errMsg = "Missing parameters\n" \
                 "Usage: %s\n" % (sys.argv[0],) + \
                 " inputImage  outputImage\n" \
                 " seedX seedY InitialDistance\n" \
                 " Sigma SigmoidAlpha SigmoidBeta\n" \
                 " PropagationScaling\n"

        print >> sys.stderr, errMsg
        return

    # We're going to build the following pipelines:
    # 1. reader -> smoothing -> gradientMagnitude -> sigmoid -> FI
    # 2. fastMarching -> geodesicActiveContour(FI) -> thresholder -> writer
    # The output of pipeline 1 is a feature image that is used by the
    # geodesicActiveContour object.  Also see figure 9.18 in the ITK
    # Software Guide.

    reader = itk.itkImageFileReaderF2_New()
    reader.SetFileName(sys.argv[1])

    writer = itk.itkImageFileWriterUS2_New()
    writer.SetFileName(sys.argv[2])

    smoothing = itk.itkCurvatureAnisotropicDiffusionImageFilterF2F2_New()

    gM = itk.itkGradientMagnitudeRecursiveGaussianImageFilterF2F2_New()
    gradientMagnitude = gM

    sigmoid = itk.itkSigmoidImageFilterF2F2_New()
    sigmoid.SetOutputMinimum(  0.0  )
    sigmoid.SetOutputMaximum(  1.0  )

    fastMarching = itk.itkFastMarchingImageFilterF2F2_New()

    gAC = itk.itkGeodesicActiveContourLevelSetImageFilterF2F2_New()
    geodesicActiveContour = gAC

    propagationScaling = float(sys.argv[9])

    geodesicActiveContour.SetPropagationScaling( propagationScaling );
    geodesicActiveContour.SetCurvatureScaling( 1.0 );
    geodesicActiveContour.SetAdvectionScaling( 1.0 );
    geodesicActiveContour.SetMaximumRMSError( 0.02 );
    geodesicActiveContour.SetNumberOfIterations( 800 );

    smoothing.SetInput( reader.GetOutput() );
    gradientMagnitude.SetInput( smoothing.GetOutput() );
    sigmoid.SetInput( gradientMagnitude.GetOutput() );

    geodesicActiveContour.SetInput(  fastMarching.GetOutput() );
    geodesicActiveContour.SetFeatureImage( sigmoid.GetOutput() );

    thresholder = itk.itkBinaryThresholdImageFilterF2US2_New()
    thresholder.SetLowerThreshold( -1000.0 );
    thresholder.SetUpperThreshold( 0.0 );
    thresholder.SetOutsideValue( 0  );
    thresholder.SetInsideValue( 65535 );

    thresholder.SetInput( geodesicActiveContour.GetOutput() );
    writer.SetInput( thresholder.GetOutput() );

    smoothing.SetTimeStep( 0.125 );
    smoothing.SetNumberOfIterations(  5 );
    smoothing.SetConductanceParameter( 3.0 );

    sigma = float(sys.argv[6])
    gradientMagnitude.SetSigma(sigma);

    alpha = float(sys.argv[7])
    beta = float(sys.argv[8])

    sigmoid.SetAlpha(alpha)
    sigmoid.SetBeta(beta)

    # same as image
    seedPosition = itk.itkIndex2()
    seedPosition.SetElement(0, int(sys.argv[3]))
    seedPosition.SetElement(1, int(sys.argv[4]))

    initialDistance = float(sys.argv[5])
    seedValue = - initialDistance
    node = itk.itkLevelSetNodeF2()
    node.SetValue(seedValue)
    node.SetIndex(seedPosition)

    seeds = itk.itkNodeContainerF2_New()
    seeds.Initialize()
    seeds.InsertElement(0, node)

    fastMarching.SetTrialPoints(seeds.GetPointer())

    fastMarching.SetSpeedConstant(1.0)

    caster1 = itk.itkRescaleIntensityImageFilterF2US2_New()
    writer1 = itk.itkImageFileWriterUS2_New()
    caster1.SetInput( smoothing.GetOutput() );
    writer1.SetInput( caster1.GetOutput() );
    writer1.SetFileName("GeodesicActiveContourImageFilterOutput1.png");
    caster1.SetOutputMinimum(   0 );
    caster1.SetOutputMaximum( 65535 );
    writer1.Update();

    caster2 = itk.itkRescaleIntensityImageFilterF2US2_New()
    writer2 = itk.itkImageFileWriterUS2_New()
    caster2.SetInput( gradientMagnitude.GetOutput() );
    writer2.SetInput( caster2.GetOutput() );
    writer2.SetFileName("GeodesicActiveContourImageFilterOutput2.png");
    caster2.SetOutputMinimum(   0 );
    caster2.SetOutputMaximum( 65535 );
    writer2.Update();

    caster3 = itk.itkRescaleIntensityImageFilterF2US2_New()
    writer3 = itk.itkImageFileWriterUS2_New()
    caster3.SetInput( sigmoid.GetOutput() );
    writer3.SetInput( caster3.GetOutput() );
    writer3.SetFileName("GeodesicActiveContourImageFilterOutput3.png");
    caster3.SetOutputMinimum(   0 );
    caster3.SetOutputMaximum( 65535 );
    writer3.Update();

    caster4 = itk.itkRescaleIntensityImageFilterF2US2_New()
    writer4 = itk.itkImageFileWriterUS2_New()
    caster4.SetInput( fastMarching.GetOutput() );
    writer4.SetInput( caster4.GetOutput() );
    writer4.SetFileName("GeodesicActiveContourImageFilterOutput4.png");
    caster4.SetOutputMinimum(   0 );
    caster4.SetOutputMaximum( 65535 );

    fastMarching.SetOutputSize(
        reader.GetOutput().GetBufferedRegion().GetSize())

    writer.Update()

    # Print out some useful information
    print "\n"
    print "Max. no. iterations: %d" % (gAC.GetNumberOfIterations())
    print "Max. RMS error: %.3f" % (gAC.GetMaximumRMSError())
    print "No. elapsed iterations: %d" % (gAC.GetElapsedIterations())
    print "RMS change: %.3f" % (gAC.GetRMSChange())

    writer4.Update()

    mapWriter = itk.itkImageFileWriterF2_New()
    mapWriter.SetInput(fastMarching.GetOutput())
    mapWriter.SetFileName("GeodesicActiveContourImageFilterOutput4.mha")
    mapWriter.Update()

    speedWriter = itk.itkImageFileWriterF2_New()
    speedWriter.SetInput( sigmoid.GetOutput() )
    speedWriter.SetFileName("GeodesicActiveContourImageFilterOutput3.mha")
    speedWriter.Update()

    gradientWriter = itk.itkImageFileWriterF2_New()
    gradientWriter.SetInput( gradientMagnitude.GetOutput() )
    gradientWriter.SetFileName("GeodesicActiveContourImageFilterOutput2.mha")
    gradientWriter.Update();

if __name__ == "__main__":
    main()
