#
#  Example on the use of the LaplacianImageFilter
#

from InsightToolkit import *

from sys import argv

reader = itkImageFileReaderF2_New()
writer = itkImageFileWriterUC2_New()

filter  = itkLaplacianImageFilterF2F2_New()

caster  = itkRescaleIntensityImageFilterF2UC2_New()

caster.SetOutputMinimum(   0 );
caster.SetOutputMaximum( 255 );

filter.SetInput( reader.GetOutput() )
caster.SetInput( filter.GetOutput() )
writer.SetInput( caster.GetOutput() )

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

writer.Update()



