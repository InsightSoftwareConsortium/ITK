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
    print 'Usage: ImageRegistration3.py fixedImageFile  movingImageFile outputImagefile'
    exit()


#
#  Define data types
#
FixedImageType  = itk.Image[itk.F, 2]
MovingImageType = itk.Image[itk.F, 2]
OutputImageType = itk.Image[itk.UC, 2]
TransformType   = itk.TranslationTransform[itk.D, 2]


#
#  Read the fixed and moving images using filenames
#  from the command line arguments
#
fixedImageReader  = itk.ImageFileReader[FixedImageType].New()
movingImageReader = itk.ImageFileReader[MovingImageType].New()

fixedImageReader.SetFileName(  argv[1] )
movingImageReader.SetFileName( argv[2] )

fixedImageReader.Update()
movingImageReader.Update()

fixedImage  = fixedImageReader.GetOutput()
movingImage = movingImageReader.GetOutput()


#
#  Instantiate the classes for the registration framework
#
registration = itk.ImageRegistrationMethod[FixedImageType, MovingImageType].New()
imageMetric  = itk.MeanSquaresImageToImageMetric[FixedImageType, MovingImageType].New()
transform    = TransformType.New()
optimizer    = itk.RegularStepGradientDescentOptimizer.New()
interpolator = itk.LinearInterpolateImageFunction[FixedImageType, itk.D].New()

registration.SetOptimizer(    optimizer    )
registration.SetTransform(    transform    )
registration.SetInterpolator( interpolator )
registration.SetMetric(       imageMetric  )

registration.SetFixedImage(  fixedImage  )
registration.SetMovingImage( movingImage )

registration.SetFixedImageRegion( fixedImage.GetBufferedRegion() )

transform.SetIdentity()
initialParameters = transform.GetParameters()

registration.SetInitialTransformParameters( initialParameters )


#
# Iteration Observer
#
def iterationUpdate():
    currentParameter = transform.GetParameters()
    print "M: %f   P: %f %f " % ( optimizer.GetValue(),
                        currentParameter.GetElement(0),
                        currentParameter.GetElement(1) )

iterationCommand = itk.PyCommand.New()
iterationCommand.SetCommandCallable( iterationUpdate )
optimizer.AddObserver( itk.IterationEvent(), iterationCommand )


#
#  Define optimizer parameters
#
optimizer.SetMaximumStepLength(  4.00 )
optimizer.SetMinimumStepLength(  0.01 )
optimizer.SetNumberOfIterations( 200  )

print "Starting registration"


#
#  Start the registration process
#
registration.Update()


#
# Get the final parameters of the transformation
#
finalParameters = registration.GetLastTransformParameters()

print "Final Registration Parameters "
print "Translation X =  %f" % (finalParameters.GetElement(0),)
print "Translation Y =  %f" % (finalParameters.GetElement(1),)


#
# Now, we use the final transform for resampling the
# moving image.
#
resampler = itk.ResampleImageFilter[MovingImageType, FixedImageType].New()
resampler.SetTransform( transform    )
resampler.SetInput(     movingImage  )

region = fixedImage.GetLargestPossibleRegion()

resampler.SetSize( region.GetSize() )

resampler.SetOutputSpacing( fixedImage.GetSpacing() )
resampler.SetOutputOrigin(  fixedImage.GetOrigin()  )
resampler.SetOutputDirection(  fixedImage.GetDirection()  )
resampler.SetDefaultPixelValue( 100 )

outputCast = itk.RescaleIntensityImageFilter[FixedImageType, OutputImageType].New()
outputCast.SetInput(resampler.GetOutput())


#
#  Write the resampled image
#
writer = itk.ImageFileWriter[OutputImageType].New()

writer.SetFileName( argv[3] )
writer.SetInput( outputCast.GetOutput() )
writer.Update()
