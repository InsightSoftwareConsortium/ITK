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
#  Example on the use of the MeanImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkMeanImageFilterUS2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )


sizeRadius = itkSize2()
sizeRadius.SetElement( 0, eval( argv[3] ) )
sizeRadius.SetElement( 1, eval( argv[3] ) )

filter.SetRadius( sizeRadius )

sout = itkStringStream()
filter.Print( sout.GetStream() )
print sout.GetString()

writer.Update()



