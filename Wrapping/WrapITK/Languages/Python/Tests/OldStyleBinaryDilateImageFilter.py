#
#  Example on the use of the BinaryDilateImageFilter
#

import dl, sys
sys.setdlopenflags(dl.RTLD_NOW|dl.RTLD_GLOBAL)

import BasePython
import BinaryMorphologyPython
import SimpleFiltersPython
import IOPython
from sys import argv

reader = IOPython.itkImageFileReaderIUS2.New()
reader.SetFileName( argv[1] )
kernel = BasePython.itkFlatStructuringElement2.Ball( 5 )
filter  = BinaryMorphologyPython.itkBinaryDilateImageFilterIUS2IUS2SE2.New()
filter.SetInput( reader.GetOutput() )
filter.SetDilateValue( 200 )
filter.SetKernel( kernel )
cast = SimpleFiltersPython.itkCastImageFilterIUS2IUC2.New()
cast.SetInput(filter.GetOutput())
writer = IOPython.itkImageFileWriterIUC2.New()
writer.SetInput(cast.GetOutput())
writer.SetFileName( argv[2] )

writer.Update()
