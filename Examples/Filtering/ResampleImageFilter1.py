

from InsightToolkit import *

from sys import argv


scale = itkScaleTransform2_New()

reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

parameters = scale.GetParameters()

parameters.SetElement( 0, eval( argv[3] ) )
parameters.SetElement( 1, eval( argv[3] ) )

reader.Update()

inputImage = reader.GetOutput()

size    = inputImage.GetLargestPossibleRegion().GetSize()

centralPixel = itkIndex3()  

centralPixel.SetElement( 0, size.GetElement(0) / 2 )
centralPixel.SetElement( 1, size.GetElement(1) / 2 )

centralPoint = itkPointD2() 

spacing = inputImage.GetSpacing()

interpolator = itkLinearInterpolateImageFunctionUS2D_New()

centralPoint.SetElement(0, centralPixel.GetElement(0) )
centralPoint.SetElement(1, centralPixel.GetElement(1) )

scale.SetCenter( centralPoint )
scale.SetParameters( parameters )

resampler = itkResampleImageFilterUS2US2_New()

resampler.SetInput( reader.GetOutput() )

resampler.SetTransform( scale.GetPointer() )
resampler.SetInterpolator( interpolator.GetPointer() )
resampler.SetSize( size )
resampler.SetOutputSpacing( spacing )

resampler.Update()


writer.SetInput( resampler.GetOutput() )

writer.Update()




