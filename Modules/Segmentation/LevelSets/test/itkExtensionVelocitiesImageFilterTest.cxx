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

#include "itkExtensionVelocitiesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"


// For debugging
#include "itkImageFileWriter.h"

namespace{
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 50 );
  double radius = 19.5;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += itk::Math::sqr( p[j] - center[j] );
    }
  accum = std::sqrt( accum );
  return ( accum - radius );

}

// simple velocity function to be extended
template <typename TPoint>
double
SimpleVelocity( const TPoint & p )
{
  TPoint center;
  center.Fill( 50 );

  double value;
  double x = p[0] - center[0];
  double y = p[1] - center[1];

  if ( y == 0.0 )
    {
    if( x >= 0.0 )
      {
      value = 0.0;
      }
    else
      {
      value = itk::Math::pi;
      }
    }
  else if ( x == 0.0 )
    {
    if ( y > 0.0 )
      {
      value = itk::Math::pi_over_2;
      }
    else // if ( y < 0.0 )
      {
      value = itk::Math::pi + itk::Math::pi_over_2;
      }
    }
  else
    {
    value = std::atan( y / x );
    if ( value < 0.0 )
      {
      value += itk::Math::pi;
      }

    if ( y <= 0.0 )
      {
      value += itk::Math::pi;
      }
    }

  return ( 10 * std::sin( value ) );

}

}

int itkExtensionVelocitiesImageFilterTest(int, char* [] )
{

  const unsigned int ImageDimension = 2;
  typedef float PixelType;

  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef ImageType::IndexType                 IndexType;
  typedef itk::Point<double,ImageDimension>    PointType;

  // Fill an input image with simple signed distance function
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 128 );
  ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleSignedDistance( point ) );
    ++iter;
    }

  // Squash up the level sets by mulitplying with a scalar
  typedef itk::ShiftScaleImageFilter<ImageType,ImageType> MultiplierType;
  MultiplierType::Pointer multiplier = MultiplierType::New();
  multiplier->SetInput( image );
  multiplier->SetScale( 0.5 );

  // Set up auxiliary variables
  ImageType::Pointer aux1 = ImageType::New();
  aux1->SetRegions( region );
  aux1->Allocate();

  aux1->FillBuffer( 1.0 );

  ImageType::Pointer aux2 = ImageType::New();
  aux2->SetRegions( region );
  aux2->Allocate();

  iter = Iterator( aux2, region );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    PointType point;
    aux2->TransformIndexToPhysicalPoint( iter.GetIndex(), point );
    iter.Set( SimpleVelocity( point ) );
    ++iter;
    }
/*
  {
  // For debugging
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( aux2 );
  writer->SetFileName( "input.mhd" );
  writer->Write();
  }
*/

  // Set up reinitialize level set image filter
  const unsigned int AuxDimension = 2;
  typedef itk::ExtensionVelocitiesImageFilter<ImageType,float,AuxDimension> ReinitializerType;
  ReinitializerType::Pointer reinitializer = ReinitializerType::New();
  reinitializer->SetInput( multiplier->GetOutput() );
  reinitializer->SetInputVelocityImage( aux1, 0 );
  reinitializer->SetInputVelocityImage( aux2, 1 );

  ShowProgressObject progressWatch(reinitializer);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  reinitializer->AddObserver( itk::ProgressEvent(), command);

/*
  {
  // For debugging
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reinitializer->GetOutputVelocityImage( 1 ) );
  writer->SetFileName( "output.mhd" );
  writer->Write();
  }
*/

  // Check the output
  typedef itk::Testing::ComparisonImageFilter<ImageType,ImageType> DifferenceType;
  DifferenceType::Pointer difference = DifferenceType::New();
  difference->SetTestInput( aux2 );
  difference->SetValidInput( reinitializer->GetOutputVelocityImage( 1 ) );
  difference->Update();

  // mask out the peak at near the center point
  ImageType::IndexType centerIndex;
  centerIndex.Fill( 50 - 8 );
  ImageType::SizeType centerSize;
  centerSize.Fill( 17 );
  ImageType::RegionType centerRegion;
  centerRegion.SetIndex( centerIndex );
  centerRegion.SetSize( centerSize );

  iter = Iterator( difference->GetOutput(), centerRegion );
  iter.GoToBegin();

  while( !iter.IsAtEnd() )
    {
    iter.Set( 0.0 );
    ++iter;
    }

  typedef itk::MinimumMaximumImageCalculator<ImageType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage( difference->GetOutput() );
  calculator->Compute();

  double maxAbsDifference = calculator->GetMaximum();
  IndexType maxAbsDifferenceIndex = calculator->GetIndexOfMaximum();

  std::cout << "Max. abs. difference = " << maxAbsDifference;
  std::cout << " at " << maxAbsDifferenceIndex << std::endl;

  if ( maxAbsDifference > 0.6 )
    {
    std::cout << "Difference above threshold of 0.6" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other member functions
  reinitializer->Print( std::cout );
  reinitializer->SetLevelSetValue( 1.0 );

  // Exercise the narrowband version
  reinitializer->SetLevelSetValue( 0.0 );
  reinitializer->NarrowBandingOn();
  reinitializer->SetNarrowBandwidth( 8 );
  reinitializer->Update();

  // We will use the output narrowband from the last run as the input narrowband
  reinitializer->SetInputNarrowBand( reinitializer->GetOutputNarrowBand() );
  reinitializer->Update();

  // Check the output by iterating through the output narrowband
  typedef ReinitializerType::NodeContainerPointer NodeContainerPointer;
  typedef ReinitializerType::NodeContainer        NodeContainerType;
  typedef NodeContainerType::ConstIterator        ContainerIterator;

  NodeContainerPointer nodes  = reinitializer->GetOutputNarrowBand();
  ContainerIterator nodeIter = nodes->Begin();
  ContainerIterator nodeEnd   = nodes->End();

  while( nodeIter != nodeEnd )
    {
    ImageType::IndexType nodeIndex = nodeIter.Value().GetIndex();
    double absDiff = itk::Math::abs( aux2->GetPixel( nodeIndex ) -
      reinitializer->GetOutputVelocityImage( 1 )->GetPixel( nodeIndex ) );
    if ( absDiff > 0.6 )
      {
      std::cout << "Abs diff: " << absDiff;
      std::cout << " at: " << nodeIndex << std::endl;
      std::cout << "Difference above threshold of 0.6" << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    nodeIter++;
    }

  // Test setting/getting velocity beyond index
  reinitializer->SetInputVelocityImage( aux1, 2 );

  if( reinitializer->GetInputVelocityImage( 2 )  )
    {
    std::cout << "GetInputVelocityImage(2) should have returned ITK_NULLPTR" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  if( reinitializer->GetOutputVelocityImage( 2 )  )
    {
    std::cout << "GetOutputVelocityImage(2) should have returned ITK_NULLPTR" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
