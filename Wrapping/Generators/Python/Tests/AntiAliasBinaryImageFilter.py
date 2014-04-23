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

from __future__ import print_function

import itk
from sys import argv, stderr, exit

itk.auto_progress(2)

if len(argv) < 3:
    print((
        "Missing Parameters \n Usage: AntiAliasBinaryImageFilter"
        " inputImage outputImage [RMS] [numberOfIterations]"), file=stderr)
    exit(1)

inputFilename = argv[1]
outputFilename = argv[2]
maximumRMSError = 0.01
numberOfIterations = 50

if len(argv) > 3:
    maximumRMSError = float(argv[3])

if len(argv) > 4:
    numberOfIterations = int(argv[4])


CharPixelType = itk.UC
RealPixelType = itk.F
Dimension = 3

CharImageType = itk.Image[CharPixelType, Dimension]
RealImageType = itk.Image[RealPixelType, Dimension]

ReaderType = itk.ImageFileReader[CharImageType]
WriterType = itk.ImageFileWriter[CharImageType]


CastToRealFilterType = itk.CastImageFilter[CharImageType, RealImageType]

RescaleFilter = itk.RescaleIntensityImageFilter[RealImageType, CharImageType]

antiAliasFilter = itk.AntiAliasBinaryImageFilter[RealImageType, RealImageType]
antiAliasFilter = antiAliasFilter.New()

reader = ReaderType.New()
writer = WriterType.New()

toReal = CastToRealFilterType.New()
rescale = RescaleFilter.New()

reader.SetFileName(inputFilename)
writer.SetFileName(outputFilename)

rescale.SetOutputMinimum(0)
rescale.SetOutputMaximum(255)

toReal.SetInput(reader.GetOutput())

antiAliasFilter.SetInput(toReal.GetOutput())

antiAliasFilter.SetMaximumRMSError(maximumRMSError)
antiAliasFilter.SetNumberOfIterations(numberOfIterations)
antiAliasFilter.SetNumberOfLayers(2)

rescale.SetInput(antiAliasFilter.GetOutput())
writer.SetInput(rescale.GetOutput())

writer.Update()
