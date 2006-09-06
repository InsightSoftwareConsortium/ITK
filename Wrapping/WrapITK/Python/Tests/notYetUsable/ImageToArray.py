from InsightToolkit import *
from numarray import *
from sys import argv

reader = itkImageFileReaderUC2_New()

connector = itkPyBufferUC2_New()

reader.SetFileName( argv[1] )

reader.Update()

print "ready to convert image into array"

buffer = connector.GetArrayFromImage( reader.GetOutput() )

writer = itkImageFileWriterUC2_New()

writer.SetFileName( argv[2] )

print "ready to convert array into image"

writer.SetInput( connector.GetImageFromArray( buffer ) )

writer.Update()


