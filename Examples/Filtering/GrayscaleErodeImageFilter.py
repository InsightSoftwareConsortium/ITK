#
#  Example on the use of the GrayscaleErodeImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUC2_New()
writer = itkImageFileWriterUC2_New()


filter  = itkGrayscaleErodeImageFilterUC2UC2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )


element = itkBinaryBallStructuringElementUC2()

element.SetRadius( 1 )
element.CreateStructuringElement()

filter.SetKernel( element )

writer.Update()



