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

sizeValues = new_ULArray(2)

ULArray_setitem( sizeValues, 0, eval( argv[3] ) )
ULArray_setitem( sizeValues, 1, eval( argv[3] ) )

print ULArray_getitem( sizeValues, 0, )
print ULArray_getitem( sizeValues, 1, )

sizeRadius.SetSize( sizeValues );

filter.SetRadius( sizeRadius )


writer.Update()



