#=========================================================================
#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    ImageRegistration4.tcl
#  Language:  C++
#  Date:      $Date$
#  Version:   $Revision$
#
#  Copyright (c) 2002 Insight Consortium. All rights reserved.
#  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
#     This software is distributed WITHOUT ANY WARRANTY; without even 
#     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
#     PURPOSE.  See the above copyright notices for more information.
#
#=========================================================================


package require InsightToolkit
package require itkinteraction

set inputFixedImageFileName  "BrainProtonDensitySliceR10X13Y17.png"
set inputMovingImageFileName  "BrainT1SliceBorder20.png"

set registration       [ itk::create ImageRegistrationMethodF2 ]
set imageMetric        [ itk::create MattesMutualInformationImageToImageMetricF2 ]
set transform          [ itk::create TranslationTransform2 ]
set optimizer          [ itk::create RegularStepGradientDescentOptimizer ]
set interpolator       [ itk::create LinearInterpolateImageFunctionF2  ]


$registration   SetOptimizer      $optimizer
$registration   SetTransform      $transform
$registration   SetInterpolator   $interpolator
$registration   SetMetric         $imageMetric


set fixedImageReader   [ itk::create ImageFileReaderF2 ]
set movingImageReader  [ itk::create ImageFileReaderF2 ]

$fixedImageReader    SetFileName  $inputFixedImageFileName 
$movingImageReader   SetFileName  $inputMovingImageFileName

$registration  SetFixedImage    [  $fixedImageReader  GetOutput  ]
$registration  SetMovingImage   [  $movingImageReader GetOutput  ]

$fixedImageReader    Update
$movingImageReader   Update

set fixedImage  [ $fixedImageReader GetOutput  ]
set movingImage [ $movingImageReader GetOutput ]

$registration  SetFixedImageRegion   [ $fixedImage  GetBufferedRegion ]

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
set resampler [itk::create ResampleImageFilterF2 ]

$resampler SetTransform $transform
$resampler SetInput     $movingImage

set region [ $fixedImage GetLargestPossibleRegion ]

$resampler SetSize  [ $region GetSize ]

$resampler SetOutputSpacing [ $fixedImage GetSpacing ]
$resampler SetOutputOrigin  [ $fixedImage GetOrigin  ]
$resampler SetDefaultPixelValue 100

set writer [ itk::create ImageFileWriterF2 ]

$writer SetFileName "ResampledImage.mhd"
$writer SetInput [ $resampler GetOutput ]
$writer Update


