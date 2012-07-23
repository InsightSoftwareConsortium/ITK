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


package require InsightToolkit
package require itkinteraction


set registration       [ itkImageRegistrationMethodF2F2_New ]
set imageMetric        [ itkMeanSquaresImageToImageMetricF2F2_New ]
set transform          [ itkTranslationTransform2_New ]
set optimizer          [ itkRegularStepGradientDescentOptimizer_New ]
set interpolator       [ itkLinearInterpolateImageFunctionF2D_New  ]


$registration   SetOptimizer     [ $optimizer GetPointer ]
$registration   SetTransform     [ $transform GetPointer ]
$registration   SetInterpolator  [ $interpolator GetPointer ]
$registration   SetMetric        [ $imageMetric GetPointer ]


set fixedImageReader   [ itkImageFileReaderF2_New ]
set movingImageReader  [ itkImageFileReaderF2_New ]

$fixedImageReader    SetFileName  [lindex $argv 0]
$movingImageReader   SetFileName  [lindex $argv 1]

$registration  SetFixedImage    [  $fixedImageReader  GetOutput  ]
$registration  SetMovingImage   [  $movingImageReader GetOutput  ]

$fixedImageReader    Update
$movingImageReader   Update

set fixedImage  [ $fixedImageReader GetOutput  ]
set movingImage [ $movingImageReader GetOutput ]

set fixedImageRegion  [ $fixedImage  GetBufferedRegion ]
$registration  SetFixedImageRegion  $fixedImageRegion

$transform SetIdentity
set initialParameters [ $transform GetParameters ]

$registration  SetInitialTransformParameters  $initialParameters



$optimizer  SetMaximumStepLength  4.00
$optimizer  SetMinimumStepLength  0.01
$optimizer  SetNumberOfIterations  200

set command [itkTclCommand_New]
$command SetInterpreter [GetInterp]
$command SetCommandString {
set currentParameter [$transform GetParameters]
puts "M: [$optimizer GetValue]  P: [$currentParameter GetElement 0 ] [$currentParameter GetElement 1 ] "}

$optimizer AddObserver [itkIterationEvent] [$command GetPointer]


# Here the registration is done
$registration Update


# Get the final parameters of the transformation
set finalParameters [$registration GetLastTransformParameters]

# Print them out
puts "Final Registration Parameters "
puts "Translation X =  [$finalParameters GetElement 0] "
puts "Translation Y =  [$finalParameters GetElement 1] "

# Now,
# we use the final transform for resampling the
# moving image.
set resampler [itkResampleImageFilterF2F2_New ]
$resampler SetTransform [$transform GetPointer]
$resampler SetInput     $movingImage

set region [ $fixedImage GetLargestPossibleRegion ]

$resampler SetSize  [ $region GetSize ]

$resampler SetOutputSpacing [ $fixedImage GetSpacing ]
$resampler SetOutputOrigin  [ $fixedImage GetOrigin  ]
$resampler SetDefaultPixelValue 100

set outputCast  [itkRescaleIntensityImageFilterF2UC2_New]
$outputCast SetOutputMinimum  0
$outputCast SetOutputMaximum 65535
$outputCast SetInput [$resampler GetOutput]


#
#  Write the resampled image
#
set writer [itkImageFileWriterUC2_New]

$writer SetFileName [lindex $argv 2]
$writer SetInput [ $outputCast GetOutput ]
$writer Update

wm withdraw .
exit
