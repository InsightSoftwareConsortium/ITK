#
#  Example on the use of the MedianImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkMedianImageFilterUS2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

sizeRadius = itkSize2()
sizeRadius.SetElement( 0, eval( argv[3] ) )
sizeRadius.SetElement( 1, eval( argv[3] ) )

filter.SetRadius( sizeRadius )


writer.Update()



