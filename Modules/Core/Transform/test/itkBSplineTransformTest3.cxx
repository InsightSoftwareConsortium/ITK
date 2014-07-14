/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"

#include "itkBSplineTransform.h"

#include <fstream>

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
class CommandProgressUpdate : public itk::Command
{
public:
  typedef  CommandProgressUpdate      Self;
  typedef  itk::Command               Superclass;
  typedef itk::SmartPointer<Self>     Pointer;
  itkNewMacro( Self );

protected:
  CommandProgressUpdate() {};

public:
  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    const itk::ProcessObject * filter = dynamic_cast< const itk::ProcessObject * >( object );
    if( ! itk::ProgressEvent().CheckEvent( &event ) )
      {
      return;
      }
    std::cout << filter->GetProgress() << std::endl;
    }
};


template <unsigned int VSplineOrder>
class BSplineTransformTest3Helper
{
public:
static int RunTest(int argc, char * argv [] )
{
  const     unsigned int   ImageDimension = 2;

  typedef   unsigned char                             PixelType;
  typedef   itk::Image< PixelType, ImageDimension >   FixedImageType;
  typedef   itk::Image< PixelType, ImageDimension >   MovingImageType;

  typedef   itk::ImageFileReader< FixedImageType  >   FixedReaderType;
  typedef   itk::ImageFileReader< MovingImageType >   MovingReaderType;

  typedef   itk::ImageFileWriter< MovingImageType >   MovingWriterType;


  typename FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typename MovingReaderType::Pointer movingReader = MovingReaderType::New();
  typename MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );


  typename FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


  typedef itk::ResampleImageFilter< MovingImageType,
                                    FixedImageType  >  FilterType;

  typename FilterType::Pointer resampler = FilterType::New();

  typedef itk::LinearInterpolateImageFunction<
                       MovingImageType, double >  InterpolatorType;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  typename FixedImageType::SpacingType   fixedSpacing    = fixedImage->GetSpacing();
  typename FixedImageType::PointType     fixedOrigin     = fixedImage->GetOrigin();
  typename FixedImageType::DirectionType fixedDirection  = fixedImage->GetDirection();

  resampler->SetOutputSpacing( fixedSpacing );
  resampler->SetOutputOrigin(  fixedOrigin  );
  resampler->SetOutputDirection(  fixedDirection  );


  typename FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  typename FixedImageType::SizeType   fixedSize =  fixedRegion.GetSize();
  resampler->SetSize( fixedSize );
  resampler->SetOutputStartIndex(  fixedRegion.GetIndex() );


  resampler->SetInput( movingReader->GetOutput() );

  movingWriter->SetInput( resampler->GetOutput() );


  const unsigned int SpaceDimension = ImageDimension;
  typedef double CoordinateRepType;

  typedef itk::BSplineTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            VSplineOrder >     TransformType;

  typename TransformType::Pointer bsplineTransform = TransformType::New();


  typedef typename TransformType::RegionType RegionType;
  RegionType bsplineRegion;
  typename RegionType::SizeType   size;

  const unsigned int numberOfGridNodesOutsideTheImageSupport = VSplineOrder;

  const unsigned int numberOfGridNodesInsideTheImageSupport = 5;

  const unsigned int numberOfGridNodes =
                        numberOfGridNodesInsideTheImageSupport +
                        numberOfGridNodesOutsideTheImageSupport;

  const unsigned int numberOfGridCells =
                        numberOfGridNodesInsideTheImageSupport - 1;

  size.Fill( numberOfGridNodes );
  bsplineRegion.SetSize( size );


  typedef typename TransformType::MeshSizeType MeshSizeType;
  MeshSizeType meshSize;
  meshSize.Fill( numberOfGridCells );

  typedef typename TransformType::PhysicalDimensionsType PhysicalDimensionsType;
  PhysicalDimensionsType fixedDimensions;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    fixedDimensions[d] = fixedSpacing[d] * ( fixedSize[d] - 1.0 );
    }

  bsplineTransform->SetTransformDomainOrigin( fixedOrigin );
  bsplineTransform->SetTransformDomainDirection( fixedDirection );
  bsplineTransform->SetTransformDomainPhysicalDimensions( fixedDimensions );
  bsplineTransform->SetTransformDomainMeshSize( meshSize );

  typedef typename TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters( numberOfParameters );


  std::ifstream infile;

  infile.open( argv[1] );

  for( unsigned int n=0; n < numberOfNodes; n++ )
    {
    infile >>  parameters[n];
    infile >>  parameters[n+numberOfNodes];
    }

  infile.close();


  bsplineTransform->SetParameters( parameters );


  typename CommandProgressUpdate::Pointer observer = CommandProgressUpdate::New();

  resampler->AddObserver( itk::ProgressEvent(), observer );


  resampler->SetTransform( bsplineTransform );

  try
    {
    movingWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::Vector< float, ImageDimension >        VectorType;
  typedef itk::Image< VectorType, ImageDimension >    DeformationFieldType;

  typename DeformationFieldType::Pointer field = DeformationFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedOrigin );
  field->SetSpacing( fixedSpacing );
  field->Allocate();

  typedef itk::ImageRegionIterator< DeformationFieldType > FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  typename TransformType::InputPointType  fixedPoint;
  typename TransformType::OutputPointType movingPoint;
  typename DeformationFieldType::IndexType index;

  VectorType displacement;

  while( ! fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = bsplineTransform->TransformPoint( fixedPoint );
    displacement[0] = movingPoint[0] - fixedPoint[0];
    displacement[1] = movingPoint[1] - fixedPoint[1];
    fi.Set( displacement );
    ++fi;
    }


  typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
  typename FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

  if( argc >= 6 )
    {
    fieldWriter->SetFileName( argv[5] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}
};


int itkBSplineTransformTest3( int argc, char * argv[] )
{

  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    std::cerr << "[deformationField][multithreader use #threads]" << std::endl;
    return EXIT_FAILURE;
    }

 const int numberOfThreads = atoi( argv[6] );

 int status = 0;
 switch( numberOfThreads )
   {
   case 0:
     {
     //Don't invoke MultiThreader at all.
     status |= BSplineTransformTest3Helper< 3 >::RunTest( argc, argv );
     break;
     }
   default:
     {
     //Use MultiThreader with argv[6] threads
     itk::MultiThreader::SetGlobalDefaultNumberOfThreads(numberOfThreads);
     itk::MultiThreader::SetGlobalMaximumNumberOfThreads(numberOfThreads);
     status |= BSplineTransformTest3Helper< 3 >::RunTest( argc, argv );
     break;
     }
   }

 if( status )
   {
   return EXIT_FAILURE;
   }

  return EXIT_SUCCESS;
}
