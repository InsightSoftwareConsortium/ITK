#
#  Example on the use of the GrayscaleDilateImageFilter
#  and Box strucutring element
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
reader2 = itk.ImageFileReader[OIType].New( FileName=argv[3] )
kernel = itk.FlatStructuringElement[dim].FromImageUC( reader2.GetOutput() )
filter  = itk.GrayscaleDilateImageFilter[IType, IType, kernel].New( reader,
                Kernel=kernel )
cast = itk.CastImageFilter[IType, OIType].New(filter)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()


