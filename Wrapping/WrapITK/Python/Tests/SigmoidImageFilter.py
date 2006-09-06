#
#  Example on the use of the SigmoidImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.SigmoidImageFilter[IType, IType].New( reader,
                  OutputMinimum=eval( argv[3] ),
                  OutputMaximum=eval( argv[4] ),
                  Alpha=eval( argv[5] ),
                  Beta=eval( argv[6] ),
                  )
cast = itk.CastImageFilter[IType, OIType].New(filter)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()

