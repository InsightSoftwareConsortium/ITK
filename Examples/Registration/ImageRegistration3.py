#=========================================================================
#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    ImageRegistration3.py
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

from InsightToolkit import *

from sys import argv



#
#  Read the fixed and moving images using filenames 
#  from the command line arguments
#
fixedImageReader  = itkImageFileReaderF2_New()
movingImageReader = itkImageFileReaderF2_New()

fixedImageReader.SetFileName(  argv[1] )
movingImageReader.SetFileName( argv[2] )

fixedImageReader.Update()
movingImageReader.Update()

fixedImage  = fixedImageReader.GetOutput()
movingImage = movingImageReader.GetOutput()




#
#  Instantiate the classes for the registration framework
#
registration = itkImageRegistrationMethodF2F2_New()
imageMetric  = itkMeanSquaresImageToImageMetricF2F2_New()
transform    = itkTranslationTransform2_New()
optimizer    = itkRegularStepGradientDescentOptimizer_New()
interpolator = itkLinearInterpolateImageFunctionF2D_New()




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
#  Define optimizer parameters
#
optimizer.SetMaximumStepLength(  4.00 )
optimizer.SetMinimumStepLength(  0.01 )
optimizer.SetNumberOfIterations( 200  )



#
#  Start the registration process
#

registration.StartRegistration() 


#
# Get the final parameters of the transformation
#
finalParameters = registration.GetLastTransformParameters()

print "Final Registration Parameters "
print "Translation X =  " + finalParameters.GetElement(0)
print "Translation Y =  " + finalParameters.GetElement(1)




#
# Now, we use the final transform for resampling the
# moving image.
#
resampler = itkResampleImageFilterF2_New()
resampler.SetTransform( transform    )
resampler.SetInput(     movingImage  )

region = fixedImage.GetLargestPossibleRegion()

resampler.SetSize( region.GetSize() )

resampler.SetOutputSpacing( fixedImage.GetSpacing() )
resampler.SetOutputOrigin(  fixedImage.GetOrigin()  )
resampler.SetDefaultPixelValue( 100 )


#
#  Write the resampled image
#
writer = itkImageFileWriterF2_New()

writer.SetFileName( argv[3] )
writer.SetInput( resampler.GetOutput() )
writer.Update()


