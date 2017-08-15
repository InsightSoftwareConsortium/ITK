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

#include "itkDiffeomorphicDemonsRegistrationFilter.h"

#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkVectorCastImageFilter.h"
#include "itkImageFileWriter.h"


// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template<typename TRegistration>
class DiffeomorphicDemonsShowProgressObject
{
public:
  DiffeomorphicDemonsShowProgressObject(TRegistration* o)
    {m_Process = o;}
  void ShowProgress()
    {
    std::cout << "Progress: " << m_Process->GetProgress() << "  ";
    std::cout << "Iter: " << m_Process->GetElapsedIterations() << "  ";
    std::cout << "Metric: "   << m_Process->GetMetric()   << "  ";
    std::cout << "RMSChange: " << m_Process->GetRMSChange() << "  ";
    std::cout << std::endl;
    }
  typename TRegistration::Pointer m_Process;
};


// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(
TImage * image,
double * center,
double radius,
typename TImage::PixelType foregnd,
typename TImage::PixelType backgnd )
{

  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator it( image, image->GetBufferedRegion() );
  it.GoToBegin();

  typename TImage::IndexType index;
  double r2 = itk::Math::sqr( radius );

  for(; !it.IsAtEnd(); ++it)
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += itk::Math::sqr((double) index[j] - center[j]);
      }
    if( distance <= r2 ) it.Set( foregnd );
    else it.Set( backgnd );
    }

}


// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(
TImage *input,
TImage *output )
{
  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator inIt( input, output->GetBufferedRegion() );
  Iterator outIt( output, output->GetBufferedRegion() );
  for(; !inIt.IsAtEnd(); ++inIt, ++outIt)
    {
    outIt.Set( inIt.Get() );
    }

}

int itkDiffeomorphicDemonsRegistrationFilterTest(int argc, char * argv [] )
{

  if( argc < 9 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "GradientType [0=Symmetric,1=Fixed,2=WarpedMoving,3=MappedMoving]" << std::endl;
    std::cerr << "UseFirstOrderExp [0=No,1=Yes]" << std::endl;
    std::cerr << "Intensity Difference Threshold (double)" << std::endl;
    std::cerr << "Maximum Update step length (double)" << std::endl;
    std::cerr << "Maximum number of iterations (int)" << std::endl;
    std::cerr << "Standard deviations (double)" << std::endl;
    std::cerr << "Maximum error (double)" << std::endl;
    std::cerr << "Maximum kernel width (int)" << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char                         PixelType;
  enum {ImageDimension = 2};
  typedef itk::Image<PixelType,ImageDimension>  ImageType;
  typedef itk::Vector<float,ImageDimension>     VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;
  typedef ImageType::IndexType                  IndexType;
  typedef ImageType::SizeType                   SizeType;
  typedef ImageType::RegionType                 RegionType;
  typedef ImageType::DirectionType              DirectionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  ImageType::SizeValueType sizeArray[ImageDimension] = { 128, 128 };
  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  DirectionType direction;
  direction.SetIdentity();
  direction(1,1)=-1;

  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();
  moving->SetDirection(direction);

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();
  fixed->SetDirection(direction);

  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();
  initField->SetDirection(direction);

  double center[ImageDimension];
  double radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle
  center[0] = 64; center[1] = 64; radius = 30;
  FillWithCircle<ImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  center[0] = 62; center[1] = 64; radius = 30;
  FillWithCircle<ImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  initField->FillBuffer( zeroVec );

  typedef itk::VectorCastImageFilter<FieldType,FieldType> CasterType;
  CasterType::Pointer caster = CasterType::New();
  caster->SetInput( initField );
  caster->InPlaceOff();

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;

  typedef itk::DiffeomorphicDemonsRegistrationFilter<
    ImageType,ImageType,FieldType>                      RegistrationType;

  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetInitialDisplacementField( caster->GetOutput() );

  registrator->SetMovingImage( moving );
  registrator->SetFixedImage( fixed );

  const double intensityDifferenceThreshold = atof( argv[3] );
  const double maximumUpdateStepLength = atof( argv[4] );
  const unsigned int numberOfIterations = atoi( argv[5] );
  const double standardDeviations = atof( argv[6] );
  const double maximumError = atof( argv[7] );
  const unsigned int maximumKernelWidth = atoi( argv[8] );

  registrator->SetIntensityDifferenceThreshold( intensityDifferenceThreshold );
  registrator->SetMaximumUpdateStepLength( maximumUpdateStepLength );
  registrator->SetNumberOfIterations( numberOfIterations );
  registrator->SetStandardDeviations( standardDeviations );
  registrator->SetMaximumError( maximumError );
  registrator->SetMaximumKernelWidth( maximumKernelWidth );

  const int gradientType = atoi( argv[1] );

  typedef RegistrationType::DemonsRegistrationFunctionType FunctionType;

  switch( gradientType )
    {
    case 0:
      registrator->SetUseGradientType( FunctionType::Symmetric );
      break;
    case 1:
      registrator->SetUseGradientType( FunctionType::Fixed );
      break;
    case 2:
      registrator->SetUseGradientType( FunctionType::WarpedMoving );
      break;
    case 3:
      registrator->SetUseGradientType( FunctionType::MappedMoving );
      break;
    }

  std::cout << "GradientType = " << registrator->GetUseGradientType() << std::endl;

  const int useFirstOrderExponential = atoi( argv[2] );

  if( useFirstOrderExponential == 0 )
    {
    registrator->SetUseFirstOrderExp( false );
    }
  else
    {
    registrator->SetUseFirstOrderExp( true );
    }


  // turn on inplace execution
  registrator->InPlaceOn();


  FunctionType * fptr;
  fptr = dynamic_cast<FunctionType *>(
    registrator->GetDifferenceFunction().GetPointer() );
  fptr->Print( std::cout );

  // exercise other member variables
  std::cout << "No. Iterations: " << registrator->GetNumberOfIterations() << std::endl;
  std::cout << "Max. kernel error: " << registrator->GetMaximumError() << std::endl;
  std::cout << "Max. kernel width: " << registrator->GetMaximumKernelWidth() << std::endl;

  double v[ImageDimension];
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    v[j] = registrator->GetStandardDeviations()[j];
    }
  registrator->SetStandardDeviations( v );

  typedef DiffeomorphicDemonsShowProgressObject<RegistrationType> ProgressType;

  ProgressType progressWatch(registrator);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  registrator->AddObserver( itk::ProgressEvent(), command);

  // warp moving image
  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  typedef WarperType::CoordRepType CoordRepType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();


  warper->SetInput( moving );
  warper->SetDisplacementField( registrator->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixed->GetSpacing() );
  warper->SetOutputOrigin( fixed->GetOrigin() );
  warper->SetOutputDirection( fixed->GetDirection() );
  warper->SetEdgePaddingValue( bgnd );

  warper->Print( std::cout );

  warper->Update();

  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter( fixed,
      fixed->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( warper->GetOutput(),
      fixed->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !fixedIter.IsAtEnd() )
    {
    if( fixedIter.Get() != warpedIter.Get() )
      {
      numPixelsDifferent++;
      }
    ++fixedIter;
    ++warpedIter;
    }

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer1 = WriterType::New();
  WriterType::Pointer writer2 = WriterType::New();
  WriterType::Pointer writer3 = WriterType::New();

  writer1->SetFileName("fixedImage.mha");
  writer2->SetFileName("movingImage.mha");
  writer3->SetFileName("registeredImage.mha");

  writer1->SetInput( fixed );
  writer2->SetInput( moving );
  writer3->SetInput( warper->GetOutput() );

  writer1->Update();
  writer2->Update();
  writer3->Update();

  std::cout << "Number of pixels different: " << numPixelsDifferent;
  std::cout << std::endl;

  const unsigned int maximumNumberOfDifferentPixels = atoi( argv[9] );

  if( numPixelsDifferent > maximumNumberOfDifferentPixels )
    {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
    }

  registrator->Print( std::cout );

  // -----------------------------------------------------------
  std::cout << "Test running registrator without initial deformation field.";
  std::cout << std::endl;

  bool passed = true;
  try
    {
    registrator->SetInput( ITK_NULLPTR );
    registrator->SetNumberOfIterations( 2 );
    registrator->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << "Unexpected error." << std::endl;
    std::cout << err << std::endl;
    passed = false;
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  //--------------------------------------------------------------
  std::cout << "Test exception handling." << std::endl;

  std::cout << "Test ITK_NULLPTR moving image. " << std::endl;
  passed = false;
  try
    {
    registrator->SetInput( caster->GetOutput() );
    registrator->SetMovingImage( ITK_NULLPTR );
    registrator->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
    passed = true;
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  registrator->SetMovingImage( moving );
  registrator->ResetPipeline();

  std::cout << "Test ITK_NULLPTR moving image interpolator. " << std::endl;
  passed = false;
  try
    {
    fptr = dynamic_cast<FunctionType *>(
      registrator->GetDifferenceFunction().GetPointer() );
    fptr->SetMovingImageInterpolator( ITK_NULLPTR );
    registrator->SetInput( initField );
    registrator->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected error." << std::endl;
    std::cout << err << std::endl;
    passed = true;
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;


}
