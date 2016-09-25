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

#include <fstream>
#include "itkPasteImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionSplitterMultidimensional.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkPasteImageFilterTest( int argc, char* argv[] )
{
  if(argc < 4)
    {
    std::cerr << "Usage: " << argv[0] << " DestinationImage SourceImage OutputImage\n";
    return -1;
    }

  const unsigned int                Dimension = 2;
  typedef unsigned char             PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

  itk::ImageFileReader< ImageType >::Pointer dest =
    itk::ImageFileReader< ImageType >::New();

  dest->SetFileName( argv[1] );

  itk::ImageFileReader< ImageType >::Pointer src =
    itk::ImageFileReader< ImageType >::New();

  src->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( src->Update() );

  // Create the filter
  typedef itk::PasteImageFilter< ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, PasteImageFilter, InPlaceImageFilter );

  filter->SetDestinationImage( dest->GetOutput() );
  filter->SetSourceImage( src->GetOutput() );

  FilterType::InputImageIndexType destIndex;
  destIndex[0] = 100;
  destIndex[1] = 70;
  filter->SetDestinationIndex( destIndex );
  TEST_SET_GET_VALUE( destIndex, filter->GetDestinationIndex() );

  FilterType::InputImageIndexType srcIndex;
  FilterType::InputImageSizeType srcSize;
  FilterType::InputImageRegionType srcRegion;

  srcIndex[0] = 20;
  srcIndex[1] = 40;

  srcSize[0] = 60;
  srcSize[1] = 40;

  srcRegion.SetIndex( srcIndex );
  srcRegion.SetSize( srcSize );

  filter->SetSourceRegion( srcRegion );
  TEST_SET_GET_VALUE( srcRegion, filter->GetSourceRegion() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // We'll tie this to a streamer to really exercise the paste code
  typedef itk::ImageRegionSplitterMultidimensional SplitterType;
  SplitterType::Pointer splitter = SplitterType::New();

  typedef itk::StreamingImageFilter< ImageType, ImageType > StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( filter->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 25 );
  streamer->SetRegionSplitter( splitter );

  try
    {
    streamer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  // Generate test image
  itk::ImageFileWriter< ImageType >::Pointer writer;
  writer = itk::ImageFileWriter< ImageType >::New();
  writer->SetInput( streamer->GetOutput() );
  writer->SetFileName( argv[3] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
