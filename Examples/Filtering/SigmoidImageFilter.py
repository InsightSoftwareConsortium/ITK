#
#  Example on the use of the SigmoidImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUC2_New()
writer = itkImageFileWriterUC2_New()


filter  = itkSigmoidImageFilterUC2UC2_New()

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



