#
#  Example on the use of DicomImageIO for reading a single DICOM slice, rescale
#  the intensities and save it in a different file format.
#

from InsightToolkit import *

from sys import argv

#
# Reads an image in  16bits/pixel 
# and save it as      8bits/pixel
#
reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUC2_New()

dicomIO = itkDicomImageIO_New()

reader.SetImageIO( dicomIO.GetPointer() )

filter  = itkRescaleIntensityImageFilterUS2UC2_New()

filter.SetOutputMinimum( 0 )
filter.SetOutputMaximum(255)

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

writer.Update()


