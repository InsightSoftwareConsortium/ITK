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

import itk
from sys import argv
itk.auto_progress(2)

InputPixelType = itk.F
OutputPixelType = itk.F

InputImageType = itk.Image[InputPixelType, 2]
OutputImageType = itk.Image[OutputPixelType, 2]

reader = itk.ImageFileReader[InputImageType].New(FileName=argv[1])

filter = itk.GradientMagnitudeRecursiveGaussianImageFilter[
    InputImageType, OutputImageType].New(reader, Sigma=float(argv[3]))
filter.Update()

WritePixelType = itk.UC
WriteImageType = itk.Image[WritePixelType, 2]

rescaler = itk.RescaleIntensityImageFilter[
    OutputImageType,
    WriteImageType].New(
    filter,
    OutputMinimum=0,
    OutputMaximum=255)
writer = itk.ImageFileWriter[WriteImageType].New(rescaler, FileName=argv[2])
writer.Update()
