#
#  Example on the use of the BinaryDilateImageFilter
#

from InsightToolkit import *
from sys import argv

reader = itkImageFileReaderIUC2.New()
reader.SetFileName( argv[1] )
kernel = itkFlatStructuringElement2.Ball( 5 )
filter  = itkBinaryDilateImageFilterIUC2IUC2SE2.New()
filter.SetInput( reader.GetOutput() )
filter.SetDilateValue( 200 )
filter.SetKernel( kernel )
writer = itkImageFileWriterIUC2.New()
writer.SetInput(filter.GetOutput())
writer.SetFileName( argv[2] )

writer.Update()
