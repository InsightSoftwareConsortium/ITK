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
#  Example on the use of the LaplacianImageFilter
#

from InsightToolkit import *

from sys import argv

reader = itkImageFileReaderF2_New()
writer = itkImageFileWriterUC2_New()

filter  = itkLaplacianImageFilterF2F2_New()

caster  = itkRescaleIntensityImageFilterF2UC2_New()

caster.SetOutputMinimum(   0 );
caster.SetOutputMaximum( 255 );

filter.SetInput( reader.GetOutput() )
caster.SetInput( filter.GetOutput() )
writer.SetInput( caster.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

writer.Update()



