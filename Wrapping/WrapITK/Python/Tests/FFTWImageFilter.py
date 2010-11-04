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

dim = 2
PixelType = itk.F
ImageType = itk.Image[PixelType, dim]

reader = itk.ImageFileReader[ImageType].New( FileName=argv[1] )

fftFilter = itk.FFTWRealToComplexConjugateImageFilter[PixelType, dim].New(reader)
fftFilter2 = itk.FFTWComplexConjugateToRealImageFilter[PixelType, dim].New(fftFilter)

cast = itk.CastImageFilter[ImageType, itk.Image[itk.UC, dim]].New( fftFilter2 )
itk.write(cast, argv[2])

print itk.size(fftFilter).GetElement(0), itk.size(fftFilter).GetElement(1)
