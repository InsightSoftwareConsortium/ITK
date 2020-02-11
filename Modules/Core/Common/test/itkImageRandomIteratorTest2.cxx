/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include <iostream>

#include "itkImageRandomIteratorWithIndex.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkTestingComparisonImageFilter.h"


int
itkImageRandomIteratorTest2(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing arguments " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  outputImageFile" << std::endl;
    std::cerr << "[baselineImage  differenceImage]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using PixelType = unsigned long;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  ImageType::Pointer image = ImageType::New();

  WriterType::Pointer writer = WriterType::New();

  ImageType::SizeType size;

  size[0] = 1000;
  size[1] = 1000;

  unsigned long numberOfSamples = size[0] * size[1];

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  image->SetRegions(region);
  image->Allocate(true); // initialize buffer to zero

  using RandomIteratorType = itk::ImageRandomIteratorWithIndex<ImageType>;

  RandomIteratorType it(image, region);

  it.SetNumberOfSamples(numberOfSamples);

  it.GoToBegin();

  PixelType counter = 0;

  //
  // Write down the order in which pixels are visited
  //
  while (!it.IsAtEnd())
  {
    it.Set(counter);
    ++it;
    ++counter;
  }

  writer->SetInput(image);
  writer->SetFileName(argv[1]);
  writer->Update();

  if (argc > 4)
  {

    using ReaderType = itk::ImageFileReader<ImageType>;

    ReaderType::Pointer reader = ReaderType::New();

    reader->SetFileName(argv[2]);

    using DifferencePixelType = signed long;
    using DifferenceImageType = itk::Image<DifferencePixelType, ImageDimension>;

    using DifferenceFilterType = itk::Testing::ComparisonImageFilter<ImageType, DifferenceImageType>;

    DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

    difference->SetValidInput(image);
    difference->SetTestInput(reader->GetOutput());
    difference->SetToleranceRadius(0);
    difference->SetDifferenceThreshold(0);

    using DifferenceWriterType = itk::ImageFileWriter<DifferenceImageType>;
    DifferenceWriterType::Pointer writer2 = DifferenceWriterType::New();

    writer2->SetInput(difference->GetOutput());

    try
    {
      writer2->Update();
    }
    catch (const itk::ExceptionObject & excp)
    {
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "Number of pixels with differences = ";
    std::cout << difference->GetNumberOfPixelsWithDifferences() << std::endl;
  }

  return EXIT_SUCCESS;
}
