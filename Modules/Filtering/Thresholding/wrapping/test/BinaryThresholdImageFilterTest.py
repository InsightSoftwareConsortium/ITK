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
#  Test BinaryThresholdImageFilter
#

import sys
import itk
itk.auto_progress(2)

inputImage = sys.argv[1]
outputImage = sys.argv[2]
lowerThreshold = int(sys.argv[3])
upperThreshold = int(sys.argv[4])
outsideValue = int(sys.argv[5])
insideValue = int(sys.argv[6])

PixelType = itk.UC
Dimension = 2

ImageType = itk.Image[PixelType, Dimension]

ReaderType = itk.ImageFileReader[ImageType]
reader = ReaderType.New()
reader.SetFileName(inputImage)

FilterType = itk.BinaryThresholdImageFilter[ImageType, ImageType]
thresholdFilter = FilterType.New()

thresholdFilter.SetInput(reader.GetOutput())

thresholdFilter.SetLowerThreshold(lowerThreshold)
thresholdFilter.SetUpperThreshold(upperThreshold)
thresholdFilter.SetOutsideValue(outsideValue)
thresholdFilter.SetInsideValue(insideValue)

WriterType = itk.ImageFileWriter[ImageType]
writer = WriterType.New()
writer.SetFileName(outputImage)
writer.SetInput(thresholdFilter.GetOutput())

writer.Update()
