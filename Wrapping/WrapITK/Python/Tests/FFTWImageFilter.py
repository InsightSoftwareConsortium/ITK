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
