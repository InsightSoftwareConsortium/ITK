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

reader = itkImageFileReaderF2_New()
canny  = itkCannyEdgeDetectionImageFilterF2F2_New()
rescaler = itkRescaleIntensityImageFilterF2US2_New()
writer = itkImageFileWriterUS2_New()
canny.SetInput(reader.GetOutput())
rescaler.SetInput(canny.GetOutput())
writer.SetInput(rescaler.GetOutput())

rescaler.SetOutputMinimum(0)
rescaler.SetOutputMaximum(65535)

reader.SetFileName("c:/Hoffman/InsightNew/Testing/Data/Input/cthead1.png")
writer.SetFileName("./testout.png")
writer.Update()
