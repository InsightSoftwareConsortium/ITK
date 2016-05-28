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

#include "itkHardConnectedComponentImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkImageFileWriter.h"

template< typename TPixel >
int DoIt( int argc, char* argv[], const std::string pixelType )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputImagePrefix" << std::endl;
    return EXIT_FAILURE;
    }
  const char * outputImageFileName = argv[1];

  const int height = 20;
  const int width = 20;

  const unsigned int Dimension = 2;

  typedef TPixel                             PixelType;
  typedef itk::Image< bool, Dimension >      InputImageType;
  typedef itk::Image< PixelType, Dimension > OutputImageType;
  typedef InputImageType::IndexType          IndexType;

  InputImageType::Pointer inputimg = InputImageType::New();
  IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  InputImageType::SizeType size;
  size[0] = width;
  size[1] = height;

  region.SetSize(size);
  region.SetIndex(index);

  inputimg->SetLargestPossibleRegion( region );
  inputimg->SetBufferedRegion( region );
  inputimg->SetRequestedRegion( region );
  inputimg->Allocate();

  int row, col;
  IndexType myIndex;
  for(row = 0;row<20;row++)
    {
    for(col = 0;col < 20;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,false);
      }
    }
  for(row = 0;row<15;row++)
    {
    for(col = 0;col<20;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,true);
      }
    }
  for(row = 0;row<10;row++)
    {
    for(col = 5;col<15;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,false);
      }
    }
  for(row = 0;row<7;row++)
    {
    for(col = 7;col<12;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,true);
      }
    }
  //InputImageType::IndexType Seed = {10,2};

  typedef itk::HardConnectedComponentImageFilter< InputImageType, OutputImageType > FilterType;
  typename FilterType::Pointer filter = FilterType::New();

  FilterWatcher watcher(filter);

  filter->SetInput(inputimg);
  //filter->SetObjectSeed(Seed);
  filter->Update();

  typedef itk::ImageRegionIterator<InputImageType> inputIterator;
  inputIterator it = inputIterator(inputimg, region);

  std::cout << "Input Image" << std::endl;
  it.GoToBegin();
  for(int i = 0;i < height*width; i++)
    {
    if((i%width) == 0)
      {
      std::cout << std::endl;
      }
    std::cout << ( it.Get() ? 1 : 0 );
    ++it;
    }
  std::cout << std::endl;

  typedef itk::ImageRegionIterator<OutputImageType> outputIterator;
  outputIterator ot = outputIterator(filter->GetOutput(), region);

  std::cout << std::endl << "Output Image" << std::endl;
  ot.GoToBegin();
  for(int i = 0;i < height*width; i++)
    {
    if((i%width) == 0)
      {
      std::cout << std::endl;
      }
    std::cout << ot.Get();
    ++ot;
    }

  std::cout << std::endl;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( std::string( outputImageFileName ) + pixelType + ".png" );
  writer->SetInput( filter->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkHardConnectedComponentImageFilterTest(int argc, char* argv[] )
{
  return DoIt< unsigned char >( argc, argv, "UnsignedChar" )
   || DoIt< unsigned short >( argc, argv, "UnsignedShort" );
}
