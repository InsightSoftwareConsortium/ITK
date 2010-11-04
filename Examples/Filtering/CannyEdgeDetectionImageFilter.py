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
#  Example on the use of the CannyEdgeDetectionImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderF2_New()
writer = itkImageFileWriterUC2_New()

outputCast = itkRescaleIntensityImageFilterF2UC2_New()

filter  = itkCannyEdgeDetectionImageFilterF2F2_New()

filter.SetInput(      reader.GetOutput()      )
outputCast.SetInput(  filter.GetOutput()      )
writer.SetInput(      outputCast.GetOutput()  )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

outputCast.SetOutputMinimum(    0  )
outputCast.SetOutputMaximum(  255  )

variance  = eval( argv[3] )
lowerThreshold = eval( argv[4] )
upperThreshold = eval( argv[5] )

filter.SetVariance(  variance  )
filter.SetLowerThreshold( lowerThreshold )
filter.SetUpperThreshold( upperThreshold )

writer.Update()


