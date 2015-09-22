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


#
#  Check input parameters
#  INPUTS(fixedImage):  {BrainProtonDensitySliceBorder20.png}
#  INPUTS(movingImage): {BrainProtonDensitySliceShifted13x17y.png}
#
if len(argv) < 4:
    print 'Missing Parameters'
    print 'Usage: ImageRegistration4.py fixedImageFile  movingImageFile outputImagefile'
    exit()


#
#  Define data types
#
FixedImageType   = itk.Image[itk.F, 2]
MovingImageType  = itk.Image[itk.F, 2]
TransformType    = itk.TranslationTransform[itk.D, 2]
OptimizerType    = itk.RegularStepGradientDescentOptimizerv4[itk.D]
RegistrationType = itk.ImageRegistrationMethodv4[FixedImageType,
                                                 MovingImageType]
MetricType       = itk.MattesMutualInformationImageToImageMetricv4[FixedImageType,
                                                                   MovingImageType]


#
#  Read the fixed and moving images using filenames
#  from the command line arguments
#
fixedImageReader  = itk.ImageFileReader[FixedImageType].New()
movingImageReader = itk.ImageFileReader[MovingImageType].New()

fixedImageReader.SetFileName(  argv[1])
movingImageReader.SetFileName( argv[2])

fixedImageReader.Update()
movingImageReader.Update()

fixedImage  = fixedImageReader.GetOutput()
movingImage = movingImageReader.GetOutput()


#
#  Instantiate the classes for the registration framework
#
registration = RegistrationType.New()
imageMetric  = MetricType.New()
transform    = TransformType.New()
optimizer    = OptimizerType.New()

registration.SetOptimizer(optimizer)
registration.SetMetric(imageMetric)

numberOfBins = 24

imageMetric.SetNumberOfHistogramBins( numberOfBins );
imageMetric.SetUseMovingImageGradientFilter( False );
imageMetric.SetUseFixedImageGradientFilter( False );

registration.SetFixedImage(fixedImage)
registration.SetMovingImage(movingImage)

registration.SetInitialTransform(transform)


#
#  Define optimizer parameters
#
optimizer.SetLearningRate(8.00)
optimizer.SetMinimumStepLength(0.001)
optimizer.SetNumberOfIterations(100)
optimizer.ReturnBestParametersAndValueOn();
optimizer.SetRelaxationFactor(0.8)


#
# One level registration process without shrinking and smoothing.
#
registration.SetNumberOfLevels(1)
registration.SetSmoothingSigmasPerLevel([0])
registration.SetShrinkFactorsPerLevel([1])

registration.SetMetricSamplingStrategy( RegistrationType.RANDOM );
registration.SetMetricSamplingPercentage( 0.20 );


#
# Iteration Observer
#
def iterationUpdate():
    currentParameter = registration.GetOutput().Get().GetParameters()
    print "M: %f   P: %f %f " % ( optimizer.GetValue(),
                                  currentParameter.GetElement(0),
                                  currentParameter.GetElement(1))

iterationCommand = itk.PyCommand.New()
iterationCommand.SetCommandCallable(iterationUpdate)
optimizer.AddObserver(itk.IterationEvent(),iterationCommand)

print "Starting registration"


#
#  Start the registration process
#
registration.Update()


#
# Get the final parameters of the transformation
#
finalParameters = registration.GetOutput().Get().GetParameters()

print "Final Registration Parameters "
print "Translation X =  %f" % (finalParameters.GetElement(0),)
print "Translation Y =  %f" % (finalParameters.GetElement(1),)


#
# Now, we use the final transform for resampling the
# moving image.
#
resampler = itk.ResampleImageFilter[MovingImageType,FixedImageType].New()
resampler.SetTransform(registration.GetTransform())
resampler.SetInput(movingImageReader.GetOutput())

region = fixedImage.GetLargestPossibleRegion()

resampler.SetSize(region.GetSize())
resampler.SetOutputOrigin(fixedImage.GetOrigin())
resampler.SetOutputSpacing(fixedImage.GetSpacing())
resampler.SetOutputDirection(fixedImage.GetDirection())
resampler.SetDefaultPixelValue(100)

OutputImageType  = itk.Image[itk.UC, 2]
outputCast = itk.CastImageFilter[FixedImageType, OutputImageType].New()
outputCast.SetInput(resampler.GetOutput())


#
#  Write the resampled image
#
writer = itk.ImageFileWriter[OutputImageType].New()
writer.SetFileName( argv[3] )
writer.SetInput( outputCast.GetOutput() )
writer.Update()
