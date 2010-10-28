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
#  Example on the use of the VoronoiSegmentationImageFilter.
#

from InsightToolkit import *

from sys import argv


readerInput = itkImageFileReaderUC2_New()
readerPrior = itkImageFileReaderUC2_New()

readerInput.SetFileName( argv[1] )
readerPrior.SetFileName( argv[2] )

readerInput.Update()
readerPrior.Update()

filter  = itkVoronoiSegmentationImageFilterUC2UC2UC2_New()

filter.SetInput(   readerInput.GetOutput() )
filter.TakeAPrior( readerPrior.GetOutput() )

filter.SetMeanPercentError( eval( argv[4] )  )
filter.SetSTDPercentError( eval( argv[5] )  )

writer = itkImageFileWriterUC2_New()
writer.SetFileName( argv[3] )
writer.SetInput( filter.GetOutput() )

writer.Update()

