#
#  Example on the use of the SigmoidImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()


filter  = itkSigmoidImageFilterUS2US2_New()

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

outputMinimum = eval( argv[3] )
outputMaximum = eval( argv[4] )

filter.SetOutputMinimum( outputMinimum )
filter.SetOutputMaximum( outputMaximum )

alpha  = eval( argv[5] )
beta   = eval( argv[6] )

filter.SetAlpha( alpha  )
filter.SetBeta(   beta  )


writer.Update()



