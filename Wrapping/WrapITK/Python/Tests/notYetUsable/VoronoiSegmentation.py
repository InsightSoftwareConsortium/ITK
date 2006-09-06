#
#  Example on the use of the VoronoiSegmentationImageFilter.
#

from InsightToolkit import *

from sys import argv


readerInput = itkImageFileReaderUC2_New()
readerPrior = itkImageFileReaderUC2_New()

readerInput.SetFileName( argv[1] )
readerPrior.SetFileName( argv[2] )

readerInput.Update()
readerPrior.Update()

filter  = itkVoronoiSegmentationImageFilterUC2UC2UC2_New()

filter.SetInput(   readerInput.GetOutput() )
filter.TakeAPrior( readerPrior.GetOutput() )

filter.SetMeanPercentError( eval( argv[4] )  )
filter.SetSTDPercentError( eval( argv[5] )  )

writer = itkImageFileWriterUC2_New()
writer.SetFileName( argv[3] )
writer.SetInput( filter.GetOutput() )

writer.Update()

