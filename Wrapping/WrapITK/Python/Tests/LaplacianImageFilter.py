#
#  Example on the use of the LaplacianImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.F, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.LaplacianImageFilter[IType, IType].New( reader )
cast = itk.RescaleIntensityImageFilter[IType, OIType].New(filter,
                OutputMinimum=0,
                OutputMaximum=255)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()
