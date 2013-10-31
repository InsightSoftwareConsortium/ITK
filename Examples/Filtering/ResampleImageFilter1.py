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




