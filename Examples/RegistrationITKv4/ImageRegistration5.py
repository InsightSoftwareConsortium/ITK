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
#  INPUTS(movingImage): {BrainProtonDensitySliceRotated10.png}
#
if len(argv) < 4:
    print 'Missing Parameters'
    print 'Usage: ImageRegistration3.py fixedImageFile  movingImageFile outputImagefile'
    exit()


#
#  Define data types
#
FixedImageType   = itk.Image[itk.F, 2]
MovingImageType  = itk.Image[itk.F, 2]
TransformType    = itk.CenteredRigid2DTransform[itk.D]
OptimizerType    = itk.RegularStepGradientDescentOptimizerv4[itk.D]
RegistrationType = itk.ImageRegistrationMethodv4[FixedImageType,
                                                 MovingImageType]
MetricType       = itk.MeanSquaresImageToImageMetricv4[FixedImageType,
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

registration.SetFixedImage(fixedImage)
registration.SetMovingImage(movingImage)


#
# Initial transform parameters
#
transform.SetAngle( 0.0 )

# center of the fixed image
fixedSpacing = fixedImage.GetSpacing()
fixedOrigin = fixedImage.GetOrigin()
fixedSize = fixedImage.GetLargestPossibleRegion().GetSize()

centerFixed = ( fixedOrigin.GetElement(0) + fixedSpacing.GetElement(0) * fixedSize.GetElement(0) / 2.0,
                fixedOrigin.GetElement(1) + fixedSpacing.GetElement(1) * fixedSize.GetElement(1) / 2.0 )

# center of the moving image
movingSpacing = movingImage.GetSpacing()
movingOrigin = movingImage.GetOrigin()
movingSize = movingImage.GetLargestPossibleRegion().GetSize()

centerMoving = ( movingOrigin.GetElement(0) + movingSpacing.GetElement(0) * movingSize.GetElement(0) / 2.0,
                 movingOrigin.GetElement(1) + movingSpacing.GetElement(1) * movingSize.GetElement(1) / 2.0  )

# transform center
center = transform.GetCenter()
center.SetElement( 0, centerFixed[0] )
center.SetElement( 1, centerFixed[1] )

# transform translation
translation = transform.GetTranslation()
translation.SetElement( 0, centerMoving[0] - centerFixed[0] )
translation.SetElement( 1, centerMoving[1] - centerFixed[1] )

registration.SetInitialTransform(transform)

initialParameters = transform.GetParameters()

print "Initial Parameters: "
print "Angle: %f" % (initialParameters.GetElement(0), )
print "Center: %f, %f" % ( initialParameters.GetElement(1), initialParameters.GetElement(2) )
print "Translation: %f, %f" % (initialParameters.GetElement(3), initialParameters.GetElement(4))


#
# Define optimizer parameters
#

# optimizer scale
translationScale = 1.0 / 1000.0

optimizerScales = itk.OptimizerParameters[itk.D](transform.GetNumberOfParameters())
optimizerScales.SetElement(0, 1.0)
optimizerScales.SetElement(1, translationScale)
optimizerScales.SetElement(2, translationScale)
optimizerScales.SetElement(3, translationScale)
optimizerScales.SetElement(4, translationScale)

optimizer.SetScales( optimizerScales )

optimizer.SetRelaxationFactor( 0.6 );
optimizer.SetLearningRate( 0.1 );
optimizer.SetMinimumStepLength( 0.001 );
optimizer.SetNumberOfIterations( 200 );


#
# One level registration process without shrinking and smoothing.
#
registration.SetNumberOfLevels(1)
registration.SetSmoothingSigmasPerLevel([0])
registration.SetShrinkFactorsPerLevel([1])


#
# Iteration Observer
#
def iterationUpdate():
    currentParameter = transform.GetParameters()
    print "M: %f   P: %f %f %f %f %f " % ( optimizer.GetValue(),
                                 currentParameter.GetElement(0),
                                 currentParameter.GetElement(1),
                                 currentParameter.GetElement(2),
                                 currentParameter.GetElement(3),
                                 currentParameter.GetElement(4) )

iterationCommand = itk.PyCommand.New()
iterationCommand.SetCommandCallable( iterationUpdate )
optimizer.AddObserver( itk.IterationEvent(), iterationCommand )

print "Starting registration"


#
# Start the registration process
#
registration.Update()


#
# Get the final parameters of the transformation
#
finalParameters = registration.GetOutput().Get().GetParameters()

print "Final Registration Parameters "
print "Angle in radians  = %f" % finalParameters.GetElement(0)
print "Rotation Center X = %f" % finalParameters.GetElement(1)
print "Rotation Center Y = %f" % finalParameters.GetElement(2)
print "Translation in  X = %f" % finalParameters.GetElement(3)
print "Translation in  Y = %f" % finalParameters.GetElement(4)


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
