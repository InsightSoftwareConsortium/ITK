#=========================================================================
#
#  Program:   Insight Segmentation & Registration Toolkit
#  Module:    ImageRegistration4.py
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
imageMetric  = itkMattesMutualInformationImageToImageMetricF2F2_New()
transform    = itkTranslationTransform2_New()
optimizer    = itkRegularStepGradientDescentOptimizer_New()
interpolator = itkLinearInterpolateImageFunctionF2D_New()


imageMetric.SetNumberOfHistogramBins( 20 );
imageMetric.SetNumberOfSpatialSamples( 10000 );

registration.SetOptimizer(    optimizer.GetPointer()    )
registration.SetTransform(    transform.GetPointer()    )
registration.SetInterpolator( interpolator.GetPointer() )
registration.SetMetric(       imageMetric.GetPointer()  )
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
 
iterationCommand = itkPyCommand_New()
iterationCommand.SetCommandCallable( iterationUpdate )
optimizer.AddObserver( itkIterationEvent(), iterationCommand.GetPointer() )



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

registration.StartRegistration() 


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
resampler = itkResampleImageFilterF2F2_New()
resampler.SetTransform( transform.GetPointer()    )
resampler.SetInput(     movingImage  )

region = fixedImage.GetLargestPossibleRegion()

resampler.SetSize( region.GetSize() )

resampler.SetOutputSpacing( fixedImage.GetSpacing() )
resampler.SetOutputOrigin(  fixedImage.GetOrigin()  )
resampler.SetDefaultPixelValue( 100 )

outputCast = itkRescaleIntensityImageFilterF2US2_New()
outputCast.SetOutputMinimum(      0  )
outputCast.SetOutputMaximum(  65535  )
outputCast.SetInput(resampler.GetOutput())

#
#  Write the resampled image
#
writer = itkImageFileWriterUS2_New()

writer.SetFileName( argv[3] )
writer.SetInput( outputCast.GetOutput() )
writer.Update()


