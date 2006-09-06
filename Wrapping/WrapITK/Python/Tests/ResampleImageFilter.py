
#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput1.png}
#     0

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput2.png}
#     1

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput3.png}
#     2

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput4.png}
#     3

import itk
from sys import argv, stderr, exit
itk.auto_progress(2)


if( len(argv) < 3 ):
  print >> stderr, """Missing Parameters
Usage: ResampleImageFilter.py inputImageFile  outputImageFile [exampleAction={0,1,2,3}]"""
  exit(1)

exampleAction = 0

if( len(argv) >= 4 ):
  exampleAction = int( argv[3] )

Dimension = 2
InputPixelType = itk.US
OutputPixelType = itk.UC
InputImageType = itk.Image[ InputPixelType,  Dimension ]
OutputImageType = itk.Image[ OutputPixelType, Dimension ]

ReaderType = itk.ImageFileReader[ InputImageType  ]
CastType = itk.CastImageFilter[ InputImageType, OutputImageType ]
WriterType = itk.ImageFileWriter[ OutputImageType ]

reader = ReaderType.New()
cast = CastType.New()
writer = WriterType.New()

reader.SetFileName( argv[1] )
writer.SetFileName( argv[2] )

FilterType = itk.ResampleImageFilter[InputImageType,InputImageType] 
filter = FilterType.New()

TransformType = itk.AffineTransform[ itk.D, Dimension ]

transform = TransformType.New()
filter.SetTransform( transform.GetPointer() )

InterpolatorType = itk.NearestNeighborInterpolateImageFunction[ 
                      InputImageType, itk.D ]

interpolator = InterpolatorType.New()
filter.SetInterpolator( interpolator.GetPointer() )

filter.SetDefaultPixelValue( 0 )

# spacing = itk.Vector[itk.D, Dimension]()
# spacing.SetElement(0, 1.0)
# spacing.SetElement(1, 1.0)
# filter.SetOutputSpacing( spacing )
filter.SetOutputSpacing( 1.0 )

# origin = itk.Point[itk.D, Dimension]()
# origin.SetElement(0, 0.0)
# origin.SetElement(1, 0.0)
# filter.SetOutputOrigin( origin )
filter.SetOutputOrigin( [0.0, 0.0] )

# size = itk.Size[Dimension]()
# size.SetElement(0, 300)
# size.SetElement(1, 300)
# filter.SetSize( size )
filter.SetSize( 300 )

filter.SetInput( reader.GetOutput() )
cast.SetInput( filter.GetOutput() )
writer.SetInput( cast.GetOutput() )
writer.Update()

# translation = itk.Vector[itk.D, Dimension]()
# translation.SetElement(0, -30)
# translation.SetElement(1, -50)
# transform.Translate( translation, 0 )
transform.Translate( [-30, -50], 0 )

if( exampleAction == 1 ):
  writer.Update()



filter.SetDefaultPixelValue( 100 )

if( exampleAction == 2 ):
  writer.Update()


