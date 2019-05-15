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

#include "itkDemonsRegistrationFilter.h"

#include "itkWarpImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkCommand.h"
#include "itkCastImageFilter.h"


namespace{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
template<typename TRegistration>
class ShowProgressObject
{
public:
  ShowProgressObject(TRegistration* o)
    {m_Process = o;}
  void ShowProgress()
    {
    std::cout << "Progress: " << m_Process->GetProgress() << "  ";
    std::cout << "Iter: " << m_Process->GetElapsedIterations() << "  ";
    std::cout << "Metric: "   << m_Process->GetMetric()   << "  ";
    std::cout << "RMSChange: " << m_Process->GetRMSChange() << "  ";
    std::cout << std::endl;
    if ( m_Process->GetElapsedIterations() == 150 )
      { m_Process->StopRegistration(); }
    }
  typename TRegistration::Pointer m_Process;
};
}

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

  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it( image, image->GetBufferedRegion() );
  it.GoToBegin();

  typename TImage::IndexType index;
  double r2 = itk::Math::sqr( radius );

  while( !it.IsAtEnd() )
    {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += itk::Math::sqr((double) index[j] - center[j]);
      }
    if( distance <= r2 ) it.Set( foregnd );
    else it.Set( backgnd );
    ++it;
    }

}


// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(
TImage *input,
TImage *output )
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator outIt( output, output->GetBufferedRegion() );
  for( Iterator inIt( input, output->GetBufferedRegion() ); !inIt.IsAtEnd(); ++inIt, ++outIt )
    {
    outIt.Set( inIt.Get() );
    }

}

int itkDemonsRegistrationFilterTest(int, char* [] )
{

  using PixelType = unsigned char;
  enum {ImageDimension = 2};
  using ImageType = itk::Image<PixelType,ImageDimension>;
  using VectorType = itk::Vector<float,ImageDimension>;
  using FieldType = itk::Image<VectorType,ImageDimension>;
  using IndexType = ImageType::IndexType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

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

  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();

  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();

  double center[ImageDimension];
  double radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle
  center[0] = 64; center[1] = 64; radius = 30;
  FillWithCircle<ImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  center[0] = 62; center[1] = 64; radius = 32;
  FillWithCircle<ImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  initField->FillBuffer( zeroVec );

  using CasterType = itk::CastImageFilter<FieldType,FieldType>;
  CasterType::Pointer caster = CasterType::New();
  caster->SetInput( initField );
  caster->InPlaceOff();

  //-------------------------------------------------------------
  std::cout << "Run registration and warp moving" << std::endl;

  using RegistrationType =
      itk::DemonsRegistrationFilter<ImageType,ImageType,FieldType>;
  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetInitialDisplacementField( caster->GetOutput() );
  registrator->SetMovingImage( moving );
  registrator->SetFixedImage( fixed );
  registrator->SetNumberOfIterations( 200 );
  registrator->SetStandardDeviations( 1.0 );
  registrator->SetMaximumError( 0.08 );
  registrator->SetMaximumKernelWidth( 10 );
  registrator->SetIntensityDifferenceThreshold( 0.001 );

  // turn on inplace execution
  registrator->InPlaceOn();

  // turn on/off use moving image gradient
  registrator->UseMovingImageGradientOff();

  using FunctionType = RegistrationType::DemonsRegistrationFunctionType;
  auto * fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer() );
  if(fptr != nullptr)
    {
    fptr->Print( std::cout );
    }

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

  using ProgressType = ShowProgressObject<RegistrationType>;
  ProgressType progressWatch(registrator);
  itk::SimpleMemberCommand<ProgressType>::Pointer command;
  command = itk::SimpleMemberCommand<ProgressType>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ProgressType::ShowProgress);
  registrator->AddObserver( itk::ProgressEvent(), command);

  // warp moving image
  using WarperType = itk::WarpImageFilter<ImageType,ImageType,FieldType>;
  WarperType::Pointer warper = WarperType::New();

  using CoordRepType = WarperType::CoordRepType;
  using InterpolatorType =
      itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>;
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

  std::cout << "Number of pixels different: " << numPixelsDifferent;
  std::cout << std::endl;

  if( numPixelsDifferent > 10 )
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
    registrator->SetInput( nullptr );
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

  std::cout << "Test nullptr moving image. " << std::endl;
  passed = false;
  try
    {
    registrator->SetInput( caster->GetOutput() );
    registrator->SetMovingImage( nullptr );
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

  std::cout << "Test nullptr moving image interpolator. " << std::endl;
  passed = false;
  try
    {
    fptr = dynamic_cast<FunctionType *>(registrator->GetDifferenceFunction().GetPointer() );
    if(fptr == nullptr)
      {
      std::cerr << "dynamic_cast failed" << std::endl;
      return EXIT_FAILURE;
      }
    fptr->SetMovingImageInterpolator( nullptr );
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
