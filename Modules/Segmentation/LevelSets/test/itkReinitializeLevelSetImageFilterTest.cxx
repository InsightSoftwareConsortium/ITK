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

#include "itkReinitializeLevelSetImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMultiplyImageFilter.h"


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

}


int itkReinitializeLevelSetImageFilterTest(int, char* [] )
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
  //multiplier->SetShift( 0.0 );

  // Set up reinitialize level set image filter
  typedef itk::ReinitializeLevelSetImageFilter<ImageType> ReinitializerType;
  ReinitializerType::Pointer reinitializer = ReinitializerType::New();
  reinitializer->SetInput( multiplier->GetOutput() );

  ShowProgressObject progressWatch(reinitializer);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  reinitializer->AddObserver( itk::ProgressEvent(), command);

  // For debugging
/*
  {
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( "input.mhd" );
  writer->Write();
  }
  {
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reinitializer->GetOutput() );
  writer->SetFileName( "output.mhd" );
  writer->Write();
  }
*/

  // Check the output signed distance map is within threshold
  typedef itk::Testing::ComparisonImageFilter<ImageType,ImageType> DifferenceType;
  DifferenceType::Pointer difference = DifferenceType::New();
  difference->SetTestInput( image );
  difference->SetValidInput( reinitializer->GetOutput() );
  difference->Update();

  typedef itk::MinimumMaximumImageCalculator<ImageType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage( difference->GetOutput() );
  calculator->Compute();

  double maxAbsDifference = calculator->GetMaximum();
  IndexType maxAbsDifferenceIndex = calculator->GetIndexOfMaximum();

  std::cout << "Max. abs. difference = " << maxAbsDifference;
  std::cout << " at " << maxAbsDifferenceIndex << std::endl;

  if ( maxAbsDifference > 1.0 )
    {
    std::cout << "Difference above threshold of 1.0" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Check if inside/outside points remain the same after reinitialization
  typedef itk::MultiplyImageFilter<ImageType,ImageType,ImageType> CheckerType;
  CheckerType::Pointer checker = CheckerType::New();
  checker->SetInput1( image );
  checker->SetInput2( reinitializer->GetOutput() );
  checker->Update();

  calculator->SetImage( checker->GetOutput() );
  calculator->Compute();
  double minValue = calculator->GetMinimum();
  double maxValue = calculator->GetMaximum();

  std::cout << "Min. product = " << minValue << std::endl;
  std::cout << "Max. product = " << maxValue << std::endl;

  if ( minValue < 0.0 )
    {
    std::cout << "Inside/Outside mismatch at ";
    std::cout << calculator->GetIndexOfMinimum() << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other member functions
  reinitializer->Print( std::cout );

  // Exercise the narrowband version
  reinitializer->SetLevelSetValue( 1.0 );
  reinitializer->SetLevelSetValue( 0.0 );
  reinitializer->NarrowBandingOn();
  reinitializer->SetNarrowBandwidth( 8 );
  reinitializer->Update();

  typedef ReinitializerType::NodeContainerPointer NodeContainerPointer;
  NodeContainerPointer nodes = reinitializer->GetOutputNarrowBand();

  std::cout << "Level set value = " << reinitializer->GetLevelSetValue() << std::endl;
  std::cout << "Narrow banding = " << reinitializer->GetNarrowBanding() << std::endl;
  std::cout << "Narrow bandwidth = " << reinitializer->GetOutputNarrowBandwidth() << std::endl;
  std::cout << "No. nodes = " << nodes->Size() << std::endl;

  // We will use the output narrowband from the last run as the input narrowband
  reinitializer->SetInputNarrowBand( nodes );
  reinitializer->Update();

  // Check if inside/outside points remain the same after reinitialization
  typedef ReinitializerType::NodeContainerPointer NodeContainerPointer;
  typedef ReinitializerType::NodeContainer        NodeContainerType;
  typedef NodeContainerType::ConstIterator        ContainerIterator;

  NodeContainerPointer nodes2  = reinitializer->GetOutputNarrowBand();
  ContainerIterator nodeIter = nodes2->Begin();
  ContainerIterator nodeEnd   = nodes2->End();

  while( nodeIter != nodeEnd )
    {
    ImageType::IndexType nodeIndex = nodeIter.Value().GetIndex();
    double product = image->GetPixel( nodeIndex ) *
      reinitializer->GetOutput()->GetPixel( nodeIndex );
    if ( product < 0.0 )
      {
      std::cout << "Product: " << product;
      std::cout << " at: " << nodeIndex << std::endl;
      std::cout << "Inside/outside mismatch" << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    nodeIter++;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
