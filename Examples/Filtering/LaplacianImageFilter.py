#
#  Example on the use of the LaplacianImageFilter
#

from InsightToolkit import *

from sys import argv

reader = itkImageFileReaderF2_New()
writer = itkImageFileWriterF2_New()

filter  = itkLaplacianImageFilterF2F2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

writer.Update()



