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


#include "itkJoinSeriesImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

#include <iostream>

class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

int itkJoinSeriesImageFilterTest( int, char* [] )
{

  const unsigned int streamDivisions = 2;
  typedef unsigned char                 PixelType;
  typedef itk::Image< PixelType, 2 >    InputImageType;
  typedef itk::Image< PixelType, 4 >    OutputImageType;

  // Expected result
  OutputImageType::IndexType expectedIndex = {{1, 2, 0, 0}};
  OutputImageType::SizeType expectedSize = {{8, 5, 4, 1}};
  OutputImageType::RegionType expectedRegion;
  expectedRegion.SetIndex( expectedIndex );
  expectedRegion.SetSize( expectedSize );
  OutputImageType::SpacingType expectedSpacing;
  expectedSpacing[0] = 1.1;
  expectedSpacing[1] = 1.2;
  expectedSpacing[2] = 1.3;
  expectedSpacing[3] = 1.0;
  OutputImageType::PointType expectedOrigin;
  expectedOrigin[0] = 0.1;
  expectedOrigin[1] = 0.2;
  expectedOrigin[2] = 0.3;
  expectedOrigin[3] = 0.0;

  // Create the input images
  int numInputs = 4;
  InputImageType::IndexType index = {{1, 2}};
  InputImageType::SizeType size = {{8, 5}};
  InputImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );
  const double spacingValue = 1.3;
  InputImageType::SpacingType spacing;
  spacing[0] = 1.1;
  spacing[1] = 1.2;
  const double originValue = 0.3;
  InputImageType::PointType origin;
  origin[0] = 0.1;
  origin[1] = 0.2;

  std::vector<InputImageType::Pointer> inputs;

  PixelType counter1 = 0;
  for ( int i = 0; i < numInputs; i++ )
    {
    inputs.push_back( InputImageType::New() );
    inputs[i]->SetLargestPossibleRegion( region );
    inputs[i]->SetBufferedRegion( region );
    inputs[i]->Allocate();

    itk::ImageRegionIterator<InputImageType>
      inputIter( inputs[i], inputs[i]->GetBufferedRegion() );
    while ( !inputIter.IsAtEnd() )
      {
      inputIter.Set( counter1 );
      ++counter1;
      ++inputIter;
      }

    inputs[i]->SetSpacing( spacing );
    inputs[i]->SetOrigin( origin );
    }

  // Create the filter
  typedef itk::JoinSeriesImageFilter< InputImageType, OutputImageType >
    JoinSeriesImageType;

  JoinSeriesImageType::Pointer joinSeriesImage = JoinSeriesImageType::New();

  EXERCISE_BASIC_OBJECT_METHODS( joinSeriesImage, JoinSeriesImageFilter,
    ImageToImageFilter );

  // Check the default values
  if ( joinSeriesImage->GetSpacing() != 1.0 )
    {
    std::cout << "Default spacing is not 1.0" << std::endl;
    return EXIT_FAILURE;
    }
  if ( joinSeriesImage->GetOrigin() != 0.0 )
    {
    std::cout << "Default origin is not 0.0" << std::endl;
    return EXIT_FAILURE;
    }

  // Setup the filter
  joinSeriesImage->SetSpacing( spacingValue );
  TEST_SET_GET_VALUE( spacingValue, joinSeriesImage->GetSpacing() );

  joinSeriesImage->SetOrigin( originValue );
  TEST_SET_GET_VALUE( originValue, joinSeriesImage->GetOrigin() );

  for ( int i = 0; i < numInputs; i++ )
    {
    joinSeriesImage->SetInput( i, inputs[i] );
    }

  // Test the ProgressReporter
  ShowProgressObject progressWatch( joinSeriesImage );
  typedef itk::SimpleMemberCommand< ShowProgressObject > CommandType;
  CommandType::Pointer command = CommandType::New();
  command->SetCallbackFunction( &progressWatch,
                                &ShowProgressObject::ShowProgress );
  joinSeriesImage->AddObserver( itk::ProgressEvent(), command );

  // Test streaming
  typedef itk::StreamingImageFilter< OutputImageType, OutputImageType >
    StreamingImageType;
  StreamingImageType::Pointer streamingImage = StreamingImageType::New();
  streamingImage->SetInput( joinSeriesImage->GetOutput() );
  streamingImage->SetNumberOfStreamDivisions( streamDivisions );

  try
    {
    streamingImage->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << err << std::endl;
    itk::DataObjectError * errp = dynamic_cast<itk::DataObjectError *>( &err );
    if ( errp )
      {
      errp->GetDataObject()->Print( std::cout );
      }
    return EXIT_FAILURE;
    }

  OutputImageType::Pointer output = streamingImage->GetOutput();


  // Check the informations
  if ( output->GetLargestPossibleRegion() != expectedRegion )
    {
    std::cout << "LargestPossibleRegion mismatch" << std::endl;
    return EXIT_FAILURE;
    }
  if ( output->GetSpacing() != expectedSpacing )
    {
    std::cout << "Spacing mismatch" << std::endl;
    return EXIT_FAILURE;
    }
  if ( output->GetOrigin() != expectedOrigin )
    {
    std::cout << "Origin mismatch" << std::endl;
    return EXIT_FAILURE;
    }

  // Check the contents
  bool passed = true;

  PixelType counter2 = 0;
  itk::ImageRegionIterator<OutputImageType>
    outputIter( output, output->GetBufferedRegion() );
  while ( !outputIter.IsAtEnd() )
    {
    if ( outputIter.Get() != counter2 )
      {
      passed = false;
      std::cout << "Mismatch at index: " << outputIter.GetIndex() << std::endl;
      }
    ++counter2;
    ++outputIter;
    }

  if ( !passed || counter1 != counter2 )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  // An exception is raised when an input is missing.
  passed = false;

  // Set the 2nd input null
  joinSeriesImage->SetInput( 1, ITK_NULLPTR );
  try
    {
    joinSeriesImage->Update();
    }
  catch( itk::InvalidRequestedRegionError & err )
    {
    std::cout << err << std::endl;
    passed = true;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  if ( !passed )
    {
    std::cout << "Expected exception is missing" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
