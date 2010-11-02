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
#  Example on the use of the GrayscaleDilateImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUC2_New()
writer = itkImageFileWriterUC2_New()


filter  = itkGrayscaleDilateImageFilterUC2UC2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )


element = itkBinaryBallStructuringElementUC2()

element.SetRadius( 1 )
element.CreateStructuringElement()

filter.SetKernel( element )

writer.Update()



