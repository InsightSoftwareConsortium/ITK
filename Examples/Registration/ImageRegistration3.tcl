#=========================================================================
#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    ImageRegistration3.tcl
#  Language:  C++
#  Date:      $Date$
#  Version:   $Revision$
#
#  Copyright (c) Insight Software Consortium. All rights reserved.
#  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#     This software is distributed WITHOUT ANY WARRANTY; without even 
#     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#     PURPOSE.  See the above copyright notices for more information.
#
#=========================================================================


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

$registration  SetFixedImageRegion  $fixedImageRegion` 

$transform SetIdentity
set initialParameters [ $transform GetParameters ]

$registration  SetInitialTransformParameters  $initialParameters 



$optimizer  SetMaximumStepLength  4.00
$optimizer  SetMinimumStepLength  0.01
$optimizer  SetNumberOfIterations  200


$optimizer AddObserver [itk::IterationEvent] [itk::createTclCommand {
  set currentParameters [$transform GetParameters]
  puts "X= [$currentParameters () 0]   Y=[$currentParameters () 1]"
}]

# Here the registration is done
$registration StartRegistration 


# Get the final parameters of the transformation
set finalParameters [$registration GetLastTransformParameters]


# Print them out
puts "Final Registration Parameters "
puts "Translation X =  [$finalParameters () 0]"
puts "Translation Y =  [$finalParameters () 1]"


# Now, 
# we use the final transform for resampling the
# moving image.
set resampler [itkResampleImageFilterF2F2_New ]

$resampler SetTransform $transform
$resampler SetInput     $movingImage

set region [ $fixedImage GetLargestPossibleRegion ]

$resampler SetSize  [ $region GetSize ]

$resampler SetOutputSpacing [ $fixedImage GetSpacing ]
$resampler SetOutputOrigin  [ $fixedImage GetOrigin  ]
$resampler SetDefaultPixelValue 100

set writer [ itkImageFileWriterF2_New ]

$writer SetFileName [lindex $argv 2]
$writer SetInput [ $resampler GetOutput ]
$writer Update


