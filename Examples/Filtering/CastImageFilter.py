#
#  Example on the use of the CastImageFilter
#

from InsightToolkit import *

from sys import argv

#
# Reads an image in   8bits/pixel 
# and save it as     16bits/pixel
#
reader = itkImageFileReaderUC2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkCastImageFilterUC2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

writer.Update()


