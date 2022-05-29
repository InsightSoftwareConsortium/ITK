/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkHardConnectedComponentImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

template <typename TPixel>
int
DoIt(int argc, char * argv[], const std::string pixelType)
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputImagePrefix" << std::endl;
    return EXIT_FAILURE;
  }
  const char * outputImageFileName = argv[1];

  constexpr int height = 20;
  constexpr int width = 20;

  constexpr unsigned int Dimension = 2;

  using PixelType = TPixel;
  using InputImageType = itk::Image<bool, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;
  using IndexType = typename InputImageType::IndexType;

  auto      inputimg = InputImageType::New();
  IndexType index;
  index.Fill(0);
  typename InputImageType::RegionType region;

  typename InputImageType::SizeType size;
  size[0] = width;
  size[1] = height;

  region.SetSize(size);
  region.SetIndex(index);

  inputimg->SetRegions(region);
  inputimg->Allocate();

  int       row, col;
  IndexType myIndex;
  for (row = 0; row < 20; ++row)
  {
    for (col = 0; col < 20; ++col)
    {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex, false);
    }
  }
  for (row = 0; row < 15; ++row)
  {
    for (col = 0; col < 20; ++col)
    {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex, true);
    }
  }
  for (row = 0; row < 10; ++row)
  {
    for (col = 5; col < 15; ++col)
    {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex, false);
    }
  }
  for (row = 0; row < 7; ++row)
  {
    for (col = 7; col < 12; ++col)
    {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex, true);
    }
  }
  // InputImageType::IndexType Seed = {10,2};

  using FilterType = itk::HardConnectedComponentImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInput(inputimg);
  // filter->SetObjectSeed(Seed);
  filter->Update();

  using inputIterator = itk::ImageRegionIterator<InputImageType>;
  inputIterator it = inputIterator(inputimg, region);

  std::cout << "Input Image" << std::endl;
  it.GoToBegin();
  for (int i = 0; i < height * width; ++i)
  {
    if ((i % width) == 0)
    {
      std::cout << std::endl;
    }
    std::cout << (it.Get() ? 1 : 0);
    ++it;
  }
  std::cout << std::endl;

  using outputIterator = itk::ImageRegionIterator<OutputImageType>;
  outputIterator ot = outputIterator(filter->GetOutput(), region);

  std::cout << std::endl << "Output Image" << std::endl;
  ot.GoToBegin();
  for (int i = 0; i < height * width; ++i)
  {
    if ((i % width) == 0)
    {
      std::cout << std::endl;
    }
    std::cout << ot.Get();
    ++ot;
  }

  std::cout << std::endl;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(std::string(outputImageFileName) + pixelType + ".png");
  writer->SetInput(filter->GetOutput());
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkHardConnectedComponentImageFilterTest(int argc, char * argv[])
{
  return DoIt<unsigned char>(argc, argv, "UnsignedChar") || DoIt<unsigned short>(argc, argv, "UnsignedShort");
}
