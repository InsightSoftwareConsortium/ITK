#
#  Example on the use of the CastImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.CastImageFilter[IType, OIType].New( reader )
writer = itk.ImageFileWriter[OIType].New( filter, FileName=argv[2] )

writer.Update()


