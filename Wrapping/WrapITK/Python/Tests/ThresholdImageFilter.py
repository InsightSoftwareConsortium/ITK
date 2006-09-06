#
#  Example on the use of the ThresholdImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.ThresholdImageFilter[IType].New( reader,
                OutsideValue=eval( argv[3] ) )
# this method can't be called in the New() method because it doesn't
# use the Set notation
filter.ThresholdAbove(  eval( argv[4] )  )

cast = itk.CastImageFilter[IType, OIType].New(filter)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()



