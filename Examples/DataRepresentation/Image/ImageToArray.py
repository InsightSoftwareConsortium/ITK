from InsightToolkit import *
from numarray import *

reader = itkImageFileReaderUC2_New()

connector = itkPyBufferUC2_New()

reader.SetFileName( argv[1] )

reader.Update()

buffer = connector.GetArrayFromImage( reader.GetOutput() )

writer = itkImageFileWriterUC2_New()

writer.SetFileName( argv[2] )

writer.SetInput( connector.GetImageFromArray( buffer ) )

writer.Update()


