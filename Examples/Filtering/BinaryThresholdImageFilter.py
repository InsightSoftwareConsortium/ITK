#
#  Example on the use of the BinaryThresholdImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkBinaryThresholdImageFilterUS2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

filter.SetLowerThreshold( eval( argv[3] )  )
filter.SetUpperThreshold( eval( argv[4] )  )
filter.SetOutsideValue(   eval( argv[5] )  )
filter.SetInsideValue(    eval( argv[6] )  )


writer.Update()



