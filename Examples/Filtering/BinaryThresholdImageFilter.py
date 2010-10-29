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
#  Example on the use of the BinaryThresholdImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkBinaryThresholdImageFilterUS2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

filter.SetLowerThreshold( eval( argv[3] )  )
filter.SetUpperThreshold( eval( argv[4] )  )
filter.SetOutsideValue(   eval( argv[5] )  )
filter.SetInsideValue(    eval( argv[6] )  )


writer.Update()



