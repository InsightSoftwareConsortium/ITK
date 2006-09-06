#
#  Example on the use of the CurvatureFlowImageFilter
#



import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.F, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
filter  = itk.CurvatureFlowImageFilter[IType, IType].New( reader,
                NumberOfIterations=eval( argv[3] ),
                TimeStep=eval( argv[4] ) )
cast = itk.RescaleIntensityImageFilter[IType, OIType].New(filter,
                OutputMinimum=0,
                OutputMaximum=255)
writer = itk.ImageFileWriter[OIType].New( cast, FileName=argv[2] )

writer.Update()

