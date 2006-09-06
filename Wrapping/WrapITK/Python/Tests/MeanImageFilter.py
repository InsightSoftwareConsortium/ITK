#
#  Example on the use of the MeanImageFilter
#

import itk
from sys import argv

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.MeanImageFilter[IType, IType].New( reader, Radius=eval( argv[3] ) )

watcher = itk.SimpleFilterWatcher( filter.GetPointer(), "filter" )

cast = itk.CastImageFilter[IType, OIType].New(filter)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()
