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
// a test routine for regional extrema using flooding
#include "itkRegionalMaximaImageFilter.h"
#include "itkHConvexImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"


int itkRegionalMaximaImageFilterTest(int argc, char * argv[])
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " Connection InputImage  OutputImageFile  "
              << "OutputImageFile2" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;

  typedef uint8_t                PixelType;
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::RegionalMaximaImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );


  // exercise Set/Get Fully connected methods for
  // testing purpose
  filter->SetFullyConnected ( true );

  if( filter->GetFullyConnected() != true )
    {
    std::cerr << "Set/Get FullyConnected error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->FullyConnectedOn();
  if( filter->GetFullyConnected() != true )
    {
    std::cerr << "Set/Get FullyConnected error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->FullyConnectedOff();
  if( filter->GetFullyConnected() != false )
    {
    std::cerr << "Set/Get FullyConnected error" << std::endl;
    return EXIT_FAILURE;
    }

  // exercise Set/Get FlatIsMaxima flag for
  // testing purpose
  filter->SetFlatIsMaxima ( true );

  if( filter->GetFlatIsMaxima() != true )
    {
    std::cerr << "Set/Get FlatIsMaxima error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->FlatIsMaximaOn();
  if( filter->GetFlatIsMaxima() != true )
    {
    std::cerr << "Set/Get FlatIsMaxima error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->FlatIsMaximaOff();
  if( filter->GetFlatIsMaxima() != false )
    {
    std::cerr << "Set/Get FlatIsMaxima error" << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetForegroundValue( 255 );
  if( filter->GetForegroundValue() != 255 )
    {
    std::cerr << "Set/Get ForegroundValue error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetBackgroundValue( 0 );
  if( filter->GetBackgroundValue() != 0 )
    {
    std::cerr << "Set/Get BackgroundValue error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetForegroundValue( itk::NumericTraits< PixelType>::max() );
  filter->SetBackgroundValue( itk::NumericTraits< PixelType>::NonpositiveMin());

  filter->SetFullyConnected( atoi(argv[1]) );
  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();


  // produce the same output with other filters
  typedef itk::HConvexImageFilter< ImageType, ImageType > ConvexType;
  ConvexType::Pointer convex = ConvexType::New();
  convex->SetInput( reader->GetOutput() );
  convex->SetFullyConnected( atoi(argv[1]) );
  convex->SetHeight( 1 );

  // convex gives maxima with value=1 and others with value=0
  // rescale the image so we have maxima=255 other=0
  typedef itk::RescaleIntensityImageFilter< ImageType, ImageType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( convex->GetOutput() );
  rescale->SetOutputMaximum( 255 );
  rescale->SetOutputMinimum( 0 );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( rescale->GetOutput() );
  writer2->SetFileName( argv[4] );
  writer2->Update();

  return 0;
}
