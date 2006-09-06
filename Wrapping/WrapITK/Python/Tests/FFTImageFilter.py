#
#  Example on the use of the MeanImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
PixelType = itk.F
ImageType = itk.Image[PixelType, dim]

reader = itk.ImageFileReader[ImageType].New( FileName=argv[1] )
fftFilter = itk.VnlFFTRealToComplexConjugateImageFilter[PixelType, dim].New(reader)

# why this Update() ?
fftFilter.Update()

ComplexImageType  = itk.Image[itk.complex[PixelType], dim]

complexWriter = itk.ImageFileWriter[ComplexImageType].New( fftFilter, FileName="complexImage.mhd" )
complexWriter.Update()

realFilter = itk.ComplexToRealImageFilter[ComplexImageType, ImageType].New( fftFilter )

WritePixelType = itk.UC
WriteImageType = itk.Image[WritePixelType, dim]
intensityRescaler = itk.RescaleIntensityImageFilter[ImageType, WriteImageType].New( realFilter, OutputMinimum=0, OutputMaximum=255 )
writer = itk.ImageFileWriter[WriteImageType].New( intensityRescaler, FileName=argv[2] )
writer.Update()

imaginaryFilter = itk.ComplexToImaginaryImageFilter[ComplexImageType, ImageType].New( fftFilter )
intensityRescaler.SetInput( imaginaryFilter.GetOutput() )
writer.SetFileName( argv[3] )
writer.Update()
