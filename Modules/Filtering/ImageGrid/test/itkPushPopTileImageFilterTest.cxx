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

int itkPushPopTileImageFilterTest(int argc, char *argv[] )
{

  typedef itk::RGBPixel<unsigned char> PixelType;
  enum { InputImageDimension = 2 };
  enum { OutputImageDimension = 2 };

  typedef itk::Image<PixelType,InputImageDimension>            InputImageType;
  typedef itk::Image<PixelType,OutputImageDimension>           OutputImageType;
  typedef itk::ImageFileReader< InputImageType >               ImageReaderType;
  typedef itk::TileImageFilter<InputImageType,OutputImageType> TilerType;
  typedef itk::ImageFileWriter<OutputImageType>                WriterType;

  if (argc != 6)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  input1 input2 input3 input4 output" << std::endl;
    return EXIT_FAILURE;
    }

  itk::FixedArray<unsigned int,2> layout;
  layout[0] = 4;
  layout[1] = 1;

  // Tile the input images
  TilerType::Pointer tiler1 = TilerType::New();
  TilerType::Pointer tiler2 = TilerType::New();
  TilerType::Pointer tiler3 = TilerType::New();
  TilerType::Pointer tiler4 = TilerType::New();
  TilerType::Pointer tiler = TilerType::New();

  unsigned char yellow[3] = {255, 255, 127};
  itk::RGBPixel<unsigned char> fillPixel = yellow;

  tiler1->SetDefaultPixelValue(fillPixel);
  tiler1->SetLayout(layout);
  tiler2->SetDefaultPixelValue(fillPixel);
  tiler2->SetLayout(layout);
  tiler3->SetDefaultPixelValue(fillPixel);
  tiler3->SetLayout(layout);
  tiler4->SetDefaultPixelValue(fillPixel);
  tiler4->SetLayout(layout);

  int f = 0;
  for (int i=1; i < argc - 1; i++)
    {
    ImageReaderType::Pointer reader = ImageReaderType::New();
    reader->SetFileName (argv[i]);
    reader->Update();
    tiler1->SetInput(f,reader->GetOutput());
    tiler2->SetInput(f,reader->GetOutput());
    tiler3->SetInput(f,reader->GetOutput());
    tiler4->SetInput(f++,reader->GetOutput());
    }

  InputImageType::ConstPointer image;

  tiler3->PopBackInput();

  image = tiler4->GetInput();
  tiler4->PopFrontInput();
  tiler4->PushBackInput(image);

  image = tiler2->GetInput(3);
  tiler2->PopBackInput();
  tiler2->PushFrontInput(image);

  layout[0] = 1;
  layout[1] = 4;
  tiler->SetDefaultPixelValue(fillPixel);
  tiler->SetLayout(layout);

  tiler->PushBackInput(tiler1->GetOutput());
  tiler->PushBackInput(tiler2->GetOutput());
  tiler->PushBackInput(tiler3->GetOutput());
  tiler->PushBackInput(tiler4->GetOutput());

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(  argv[argc-1] );

  try
    {
    writer->SetInput(tiler->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing file." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
