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

set inputFixedImageFileName  "$env(ITK_DATA_ROOT)/Input/cthead1.png"
set inputMovingImageFileName  "$env(ITK_DATA_ROOT)/Input/cthead1.png"

set registrationMethod [ itk::create ImageRegistrationMethodF3 ]
set imageMetric        [ itk::create MeanSquaresImageToImageMetricF3 ]
set transform          [ itk::create TranslationTransform3 ]
set optimizer          [ itk::create RegularStepGradientDescentOptimizer ]
set interpolator       [ itk::create LinearInterpolateImageFunctionF3  ]


$registration   SetOptimizer      $optimizer
$registration   SetTransform      $transform
$registration   SetInterpolator   $interpolator
$registration   SetMetric         $metric


set fixedImageReader   [ itk::create ImageFileReaderF3 ]
set movingImageReader  [ itk::create ImageFileReaderF3 ]

$fixedImageReader    SetFileName  $inputFixedImageFileName 
$movingImageReader   SetFileName  $inputMovingImageFileName

$registration  SetFixedImage    [  $fixedImageReader  GetOutput  ]
$registration  SetMovingImage   [  $movingImageReader GetOutput  ]

$fixedImageReader    Update
$movingImageReader   Update

set fixedImage  [ $fixedImageReader GetOutput  ]
set movingImage [ $movingImageReader GetOutput ]

$registration  SetFixedImageRegion   [ $fixedImage  GetBufferedRegion ]
$registration  SetMovingImageRegion  [ $movingImage GetBufferedRegion ]
  

set initialParameters [ ArrayD ]

$initialParameters Fill 0.0

$registration  SetInitialTransformParameters  $initialParameters 



$optimizer  SetMaximumStepLength  4.00
$optimizer  SetMinimumStepLength  0.01
$optimizer  SetNumberOfIterations  200



# Here the registration is done
$registration StartRegistration 



# Now, 
# we use the final transform for resampling the
# moving image.
$resampler [itk::create ResampleFilterTypeF3 ]

$resampler SetTransform $transform
$resampler SetInput     $movingImage

set region [ $fixedImage GetLargestPossibleRegion ]

$resampler SetSize  $region GetSize
 
$resampler SetOutputSpacing [ $fixedImage GetSpacing ]
$resampler SetOutputOrigin  [ $fixedImage GetOrigin  ]
$resampler SetDefaultPixelValue 100

set writer [ itk::create ImageFileWriterF3 ]

$writer SetFileName "ResampledImage.mhd"
$writer SetInput [ $resampler GetOutput ]
$writer Update


