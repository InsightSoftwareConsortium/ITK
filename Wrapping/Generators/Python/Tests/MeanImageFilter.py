
#
#  Example on the use of the MeanImageFilter
#

import itk
from sys import argv

dim = 2
IType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.MeanImageFilter[IType, IType].New( reader, Radius=eval( argv[3] ) )

watcher = itk.SimpleFilterWatcher( filter, "filter" )

writer = itk.ImageFileWriter[IType].New( filter, FileName=argv[2] )

writer.Update()
