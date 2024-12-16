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

#include "itkCentralDifferenceImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

int
itkCentralDifferenceImageFunctionSpeedTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " size reps doEAI doEACI doE" << std::endl;
    return EXIT_FAILURE;
  }

  const int  imageSize = std::stoi(argv[1]);
  const int  reps = std::stoi(argv[2]);
  const bool doEAI = std::stoi(argv[3]);
  const bool doEACI = std::stoi(argv[4]);
  const bool doE = std::stoi(argv[5]);

  std::cout << "imageSize: " << imageSize << " reps: " << reps << " doEAI, doEACI, doE: " << doEAI << ", " << doEACI
            << ", " << doE << std::endl;

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned int;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto                        image = ImageType::New();
  auto                        size = ImageType::SizeType::Filled(imageSize);
  const ImageType::RegionType region(size);

  image->SetRegions(region);
  image->Allocate();

  // make a test image
  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(image, region);
  iter.GoToBegin();
  unsigned int counter = 0;

  while (!iter.IsAtEnd())
  {
    iter.Set(counter);
    ++counter;
    ++iter;
  }

  // set up central difference calculator
  using CoordinateType = float;
  using FunctionType = itk::CentralDifferenceImageFunction<ImageType, CoordinateType>;
  using OutputType = FunctionType::OutputType;

  auto function = FunctionType::New();

  function->SetInputImage(image);

  OutputType total{};

  std::cout << "UseImageDirection: " << function->GetUseImageDirection() << std::endl;

  /// loop
  for (int l = 0; l < reps; ++l)
  {
    iter.GoToBegin();
    while (!iter.IsAtEnd())
    {
      ImageType::IndexType index = iter.GetIndex();
      if (doEAI)
      {
        const OutputType indexOutput = function->EvaluateAtIndex(index);
        total += indexOutput;
      }

      if (doEACI)
      {
        FunctionType::ContinuousIndexType cindex;
        cindex[0] = index[0] + 0.1;
        cindex[1] = index[1] + 0.1;
        const OutputType continuousIndexOutput = function->EvaluateAtContinuousIndex(cindex);
        total += continuousIndexOutput;
      }

      if (doE)
      {
        FunctionType::PointType point;
        image->TransformIndexToPhysicalPoint(index, point);
        const OutputType pointOutput = function->Evaluate(point);
        total += pointOutput;
      }

      ++iter;
    }
  }
  std::cout << "total: " << total << std::endl;

  return EXIT_SUCCESS;
}
