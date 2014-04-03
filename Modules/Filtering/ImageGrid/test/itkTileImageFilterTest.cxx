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

#include "itkTileImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkFilterWatcher.h"

int itkTileImageFilterTest(int argc, char *argv[] )
{

  typedef itk::RGBPixel<unsigned char> PixelType;
  enum { InputImageDimension = 2 };
  enum { OutputImageDimension = 3 };

  typedef itk::Image<PixelType,InputImageDimension>              InputImageType;
  typedef itk::Image<PixelType,OutputImageDimension>             OutputImageType;
  typedef itk::ImageFileReader< InputImageType >                 ImageReaderType;
  typedef itk::TileImageFilter<InputImageType,OutputImageType>   TilerType;
  typedef itk::ImageSeriesWriter<OutputImageType,InputImageType> WriterType;

  if (argc < 6)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  iSize jSize kSize input1 input2 ... inputn output" << std::endl;
    return EXIT_FAILURE;
    }

  itk::FixedArray<unsigned int,3> layout;
  layout[0] = atoi(argv[1]);
  layout[1] = atoi(argv[2]);
  layout[2] = atoi(argv[3]);

  // Tile the input images
  TilerType::Pointer tiler = TilerType::New();
  FilterWatcher tileWatcher(tiler, "Tiler");
  int f = 0;
  for (int i=4; i < argc - 1; i++)
    {
    ImageReaderType::Pointer reader = ImageReaderType::New();
    reader->SetFileName (argv[i]);
    reader->Update();
    tiler->SetInput(f++,reader->GetOutput());
    }
  tiler->SetLayout(layout);
  unsigned char yellow[3] = {255, 255, 127};
  itk::RGBPixel<unsigned char> fillPixel = yellow;
  tiler->SetDefaultPixelValue(fillPixel);
  tiler->Update();
  tiler->GetOutput()->Print(std::cout);

  tiler->Print( std::cout );

  WriterType::Pointer writer = WriterType::New();

  writer->SetSeriesFormat (  argv[argc-1] );

  try
    {
    writer->SetInput(tiler->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with SeriesFileNames generator" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
