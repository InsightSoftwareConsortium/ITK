#
#  Example on the use of the GradientAnisotropicDiffusionImageFilter
#

from InsightToolkit import *

from sys import argv


reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUS2_New()

inputCast  = itkCastImageFilterUS2F2_New()

outputCast = itkRescaleIntensityImageFilterF2US2_New()

filter  = itkGradientAnisotropicDiffusionImageFilterF2F2_New()

inputCast.SetInput(   reader.GetOutput()      )
filter.SetInput(      inputCast.GetOutput()   )
outputCast.SetInput(  filter.GetOutput()      )
writer.SetInput(      outputCast.GetOutput()  )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

outputCast.SetOutputMinimum(      0  )
outputCast.SetOutputMaximum(  65535  )

numberOfIterations = eval( argv[3] )
timeStep           = eval( argv[4] )
conductance        = eval( argv[5] )

filter.SetNumberOfIterations(    numberOfIterations )
filter.SetTimeStep(              timeStep           )
filter.SetConductanceParameter(  conductance        )


writer.Update()


