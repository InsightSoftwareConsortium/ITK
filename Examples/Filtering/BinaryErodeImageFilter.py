#
#  Example on the use of the BinaryErodeImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUC2_New()
writer = itkImageFileWriterUC2_New()


filter  = itkBinaryErodeImageFilterUC2UC2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )


element = itkBinaryBallStructuringElementUC2()

element.SetRadius( 1 )
element.CreateStructuringElement()

filter.SetKernel( element )

filter.SetErodeValue( 255 )

writer.Update()



