#
#  Example on the use of the SmoothingRecursiveGaussianImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderF2_New()
writer = itkImageFileWriterUS2_New()

outputCast = itkRescaleIntensityImageFilterF2US2_New()

filter  = itkSmoothingRecursiveGaussianImageFilterF2F2_New()

filter.SetInput(      reader.GetOutput()   )
outputCast.SetInput(  filter.GetOutput()      )
writer.SetInput(      outputCast.GetOutput()  )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

outputCast.SetOutputMinimum(      0  )
outputCast.SetOutputMaximum(  65535  )

filter.SetSigma( eval( argv[3] ) )

writer.Update()


