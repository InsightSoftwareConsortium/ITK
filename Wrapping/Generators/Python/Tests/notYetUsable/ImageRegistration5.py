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

#

from InsightToolkit import *

from sys import argv

#
# Read the fixed and moving images using filenames
# from the command line arguments
#
fixedImageReader = itkImageFileReaderF2_New()
movingImageReader = itkImageFileReaderF2_New()

fixedImageReader.SetFileName(argv[1])
movingImageReader.SetFileName(argv[2])

fixedImageReader.Update()
movingImageReader.Update()

fixedImage = fixedImageReader.GetOutput()
movingImage = movingImageReader.GetOutput()

#
#  Instantiate the classes for the registration framework
#
registration = itkImageRegistrationMethodF2F2_New()
imageMetric = itkMeanSquaresImageToImageMetricF2F2_New()
transform = itkCenteredRigid2DTransform_New()
optimizer = itkRegularStepGradientDescentOptimizer_New()
interpolator = itkLinearInterpolateImageFunctionF2D_New()

registration.SetOptimizer(optimizer.GetPointer())
registration.SetTransform(transform.GetPointer())
registration.SetInterpolator(interpolator.GetPointer())
registration.SetMetric(imageMetric.GetPointer())
registration.SetFixedImage(fixedImage)
registration.SetMovingImage(movingImage)
registration.SetFixedImageRegion(fixedImage.GetBufferedRegion())


#
# Initial transform parameters
#
transform.SetAngle(0.0)

# center of the fixed image
fixedSpacing = fixedImage.GetSpacing()
fixedOrigin = fixedImage.GetOrigin()
fixedSize = fixedImage.GetLargestPossibleRegion().GetSize()

centerFixed = (
    fixedOrigin.GetElement(0) + fixedSpacing.GetElement(0) *
    fixedSize.GetElement(0) / 2.0,
    fixedOrigin.GetElement(1) + fixedSpacing.GetElement(1) *
    fixedSize.GetElement(1) / 2.0)

# center of the moving image
movingSpacing = movingImage.GetSpacing()
movingOrigin = movingImage.GetOrigin()
movingSize = movingImage.GetLargestPossibleRegion().GetSize()

centerMoving = (
    movingOrigin.GetElement(0) + movingSpacing.GetElement(0) *
    movingSize.GetElement(0) / 2.0,
    movingOrigin.GetElement(1) + movingSpacing.GetElement(1) *
    movingSize.GetElement(1) / 2.0)

# transform center
center = transform.GetCenter()
center.SetElement(0, centerFixed[0])
center.SetElement(1, centerFixed[1])

# transform translation
translation = transform.GetTranslation()
translation.SetElement(0, centerMoving[0] - centerFixed[0])
translation.SetElement(1, centerMoving[1] - centerFixed[1])

initialParameters = transform.GetParameters()

print "Initial Parameters: "
print "Angle: %f" % (initialParameters.GetElement(0), )
print "Center: %f, %f" % (initialParameters.GetElement(1),
                          initialParameters.GetElement(2))
print "Translation: %f, %f" % (initialParameters.GetElement(3),
                               initialParameters.GetElement(4))

registration.SetInitialTransformParameters(initialParameters)

#
# Define optimizer parameters
#

# optimizer scale
translationScale = 1.0 / 1000.0

optimizerScales = itkArrayD(transform.GetNumberOfParameters())
optimizerScales.SetElement(0, 1.0)
optimizerScales.SetElement(1, translationScale)
optimizerScales.SetElement(2, translationScale)
optimizerScales.SetElement(3, translationScale)
optimizerScales.SetElement(4, translationScale)

optimizer.SetScales(optimizerScales)
optimizer.SetMaximumStepLength(0.1)
optimizer.SetMinimumStepLength(0.001)
optimizer.SetNumberOfIterations(200)

#
# Iteration Observer
#


def iterationUpdate():
    currentParameter = transform.GetParameters()
    print "M: %f   P: %f %f %f %f %f " % (optimizer.GetValue(),
                                          currentParameter.GetElement(0),
                                          currentParameter.GetElement(1),
                                          currentParameter.GetElement(2),
                                          currentParameter.GetElement(3),
                                          currentParameter.GetElement(4))

iterationCommand = itkPyCommand_New()
iterationCommand.SetCommandCallable(iterationUpdate)
optimizer.AddObserver(itkIterationEvent(), iterationCommand.GetPointer())

print "Starting registration"

#
# Start the registration process
#

registration.Update()

#
# Get the final parameters of the transformation
#
finalParameters = registration.GetLastTransformParameters()

print "Final Registration Parameters "
print "Angle in radians  = %f" % finalParameters.GetElement(0)
print "Rotation Center X = %f" % finalParameters.GetElement(1)
print "Rotation Center Y = %f" % finalParameters.GetElement(2)
print "Translation in  X = %f" % finalParameters.GetElement(3)
print "Translation in  Y = %f" % finalParameters.GetElement(4)

# Now, we use the final transform for resampling the moving image.
resampler = itkResampleImageFilterF2F2_New()

resampler.SetTransform(transform.GetPointer())
resampler.SetInput(movingImage)

region = fixedImage.GetLargestPossibleRegion()

resampler.SetSize(region.GetSize())
resampler.SetOutputSpacing(fixedImage.GetSpacing())
resampler.SetOutputOrigin(fixedImage.GetOrigin())
resampler.SetDefaultPixelValue(100)

#
# Cast for output
#
outputCast = itkRescaleIntensityImageFilterF2US2_New()
outputCast.SetInput(resampler.GetOutput())
outputCast.SetOutputMinimum(0)
outputCast.SetOutputMaximum(65535)

writer = itkImageFileWriterUS2_New()

writer.SetFileName(argv[3])
writer.SetInput(outputCast.GetOutput())
writer.Update()
