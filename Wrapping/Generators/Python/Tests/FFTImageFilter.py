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

import itk
from sys import argv
import os
itk.auto_progress(2)

dim = 2
PixelType = itk.F
ImageType = itk.Image[PixelType, dim]
ComplexImageType = itk.Image[itk.complex[PixelType], dim]

reader = itk.ImageFileReader[ImageType].New(FileName=argv[1])
fftFilter = itk.ForwardFFTImageFilter[ImageType, ComplexImageType].New(reader)

# why this Update() ?
fftFilter.Update()

complexWriter = itk.ImageFileWriter[ComplexImageType].New(
    fftFilter,
    FileName=os.path.join(os.path.dirname(argv[2]), "complexImage.mhd"))
complexWriter.Update()

realFilter = itk.ComplexToRealImageFilter[
    ComplexImageType, ImageType].New(fftFilter)

WritePixelType = itk.UC
WriteImageType = itk.Image[WritePixelType, dim]
intensityRescaler = itk.RescaleIntensityImageFilter[
    ImageType,
    WriteImageType].New(
    realFilter,
    OutputMinimum=0,
    OutputMaximum=255)
writer = itk.ImageFileWriter[WriteImageType].New(
    intensityRescaler,
    FileName=argv[2])
writer.Update()

imaginaryFilter = itk.ComplexToImaginaryImageFilter[
    ComplexImageType, ImageType].New(fftFilter)
intensityRescaler.SetInput(imaginaryFilter.GetOutput())
writer.SetFileName(argv[3])
writer.Update()
