import itk
from sys import argv, stderr, exit

itk.auto_progress(2)

if len(argv) < 3 :
  print >> stderr, """Missing Parameters
Usage: AntiAliasBinaryImageFilter inputImage outputImage [RMS] [numberOfIterations]"""
  exit(1)

inputFilename  = argv[1]
outputFilename = argv[2]
maximumRMSError = 0.01
numberOfIterations = 50

if len(argv) > 3 :
  maximumRMSError = float( argv[3] )

if len(argv) > 4 :
  numberOfIterations = int( argv[4] )


CharPixelType = itk.UC
RealPixelType = itk.F
Dimension = 3

CharImageType = itk.Image[CharPixelType, Dimension]
RealImageType = itk.Image[RealPixelType, Dimension]

ReaderType = itk.ImageFileReader[ CharImageType ]
WriterType = itk.ImageFileWriter[ CharImageType ]




CastToRealFilterType = itk.CastImageFilter[ CharImageType, RealImageType] 

RescaleFilter = itk.RescaleIntensityImageFilter[RealImageType, CharImageType ] 


AntiAliasFilterType = itk.AntiAliasBinaryImageFilter[RealImageType, RealImageType] 

reader = ReaderType.New()
writer = WriterType.New()

toReal = CastToRealFilterType.New()
rescale = RescaleFilter.New()

antiAliasFilter = AntiAliasFilterType.New()

reader.SetFileName( inputFilename  )
writer.SetFileName( outputFilename )

rescale.SetOutputMinimum(   0 )
rescale.SetOutputMaximum( 255 )

toReal.SetInput( reader.GetOutput() )

antiAliasFilter.SetInput( toReal.GetOutput() )

antiAliasFilter.SetMaximumRMSError( maximumRMSError )
antiAliasFilter.SetNumberOfIterations( numberOfIterations )
antiAliasFilter.SetNumberOfLayers( 2 )

rescale.SetInput( antiAliasFilter.GetOutput() )
writer.SetInput( rescale.GetOutput() )
  
writer.Update()
