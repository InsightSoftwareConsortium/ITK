#
#  Example on the use of the BinaryThresholdImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.BinaryThresholdImageFilter[IType, IType].New( reader,
                LowerThreshold=eval( argv[3] ),
                UpperThreshold=eval( argv[4] ),
                OutsideValue=eval( argv[5] ),
                InsideValue=eval( argv[6] ))
writer = itk.ImageFileWriter[IType].New( filter, FileName=argv[2] )

writer.Update()

