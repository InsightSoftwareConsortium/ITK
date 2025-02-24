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

#include "itkCollidingFrontsImageFilter.h"

#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkCollidingFrontsImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " negativeEpsilon applyConnectivity stopOnTargets [outputFilename]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;
  using InternalPixelType = float;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using InternalImageType = itk::Image<InternalPixelType, ImageDimension>;

  // setup uniform image

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize(imageSize);

  constexpr PixelType background = 64;

  auto inputImage = ImageType::New();
  inputImage->SetRegions(imageRegion);
  inputImage->Allocate();
  inputImage->FillBuffer(background);

  using CastFilterType = itk::CastImageFilter<ImageType, InternalImageType>;
  auto caster = CastFilterType::New();
  caster->SetInput(inputImage);

  using CollidingFrontsFilterType = itk::CollidingFrontsImageFilter<InternalImageType, InternalImageType>;
  auto collidingFronts = CollidingFrontsFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(collidingFronts, CollidingFrontsImageFilter, ImageToImageFilter);

  using NodeContainer = CollidingFrontsFilterType::NodeContainer;
  using NodeType = typename CollidingFrontsFilterType::NodeType;

  // select seeds 20 pixels apart

  auto seeds1 = NodeContainer::New();

  InternalImageType::IndexType seedPosition1;
  seedPosition1[0] = 50;
  seedPosition1[1] = 60;

  NodeType node1;
  node1.SetIndex(seedPosition1);
  node1.SetValue(0.0);

  seeds1->Initialize();
  seeds1->InsertElement(0, node1);

  auto seeds2 = NodeContainer::New();

  InternalImageType::IndexType seedPosition2;
  seedPosition2[0] = 70;
  seedPosition2[1] = 60;

  NodeType node2;
  node2.SetIndex(seedPosition2);
  node2.SetValue(0.0);

  seeds2->Initialize();
  seeds2->InsertElement(0, node2);

  InternalImageType::OffsetType offset = { { 60, 60 } };
  const double                  radius = seedPosition2[0] - offset[0];

  collidingFronts->SetInput(caster->GetOutput());
  collidingFronts->SetSeedPoints1(seeds1);
  collidingFronts->SetSeedPoints2(seeds2);

  auto negativeEpsilon = std::stod(argv[1]);
  collidingFronts->SetNegativeEpsilon(negativeEpsilon);
  ITK_TEST_SET_GET_VALUE(negativeEpsilon, collidingFronts->GetNegativeEpsilon());

  auto applyConnectivity = static_cast<bool>(std::stoi(argv[2]));
  ITK_TEST_SET_GET_BOOLEAN(collidingFronts, ApplyConnectivity, applyConnectivity);

  ITK_TRY_EXPECT_NO_EXCEPTION(collidingFronts->Update());


  const InternalImageType::Pointer output = collidingFronts->GetOutput();

  itk::ImageRegionIterator<InternalImageType> iterator(output, output->GetBufferedRegion());

  bool passed = true;

  for (; !iterator.IsAtEnd(); ++iterator)
  {
    InternalImageType::IndexType tempIndex;
    tempIndex = iterator.GetIndex();
    tempIndex -= offset;
    double distance = 0.0;
    for (int j = 0; j < 2; ++j)
    {
      distance += tempIndex[j] * tempIndex[j];
    }
    distance = std::sqrt(distance);
    const InternalImageType::PixelType outputPixel = iterator.Get();

    // for test to pass, the circle of radius 10 centered in offset
    // must be made up only of negative pixels and vice-versa

    if (outputPixel < 0.0)
    {
      // allow half a pixel diagonal tolerance
      if (distance > radius + 1.414 / 2.0)
      {
        std::cout << outputPixel << ' ' << distance << std::endl;
        passed = false;
      }
    }
    else
    {
      if (distance < radius)
      {
        std::cout << outputPixel << ' ' << distance << std::endl;
        passed = false;
      }
    }
  }

  // Optionally writing out the two images
  if (argc > 4)
  {
    using WriterType = itk::ImageFileWriter<ImageType>;
    auto writer = WriterType::New();

    using RescaleFilterType = itk::RescaleIntensityImageFilter<InternalImageType, ImageType>;
    auto rescaler = RescaleFilterType::New();

    writer->SetFileName(argv[4]);
    writer->SetInput(inputImage);
    writer->Update();

    rescaler->SetInput(collidingFronts->GetOutput());
    rescaler->SetOutputMinimum(0);
    rescaler->SetOutputMaximum(255);

    writer->SetFileName(argv[4]);
    writer->SetInput(rescaler->GetOutput());
    writer->Update();
  }

  if (!passed)
  {
    std::cout << "Colliding Fronts test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  auto stopOnTargets = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(collidingFronts, StopOnTargets, stopOnTargets);

  ITK_TRY_EXPECT_NO_EXCEPTION(collidingFronts->Update());


  std::cout << "Colliding Fronts test passed. " << std::endl;

  using DoubleImageType = itk::Image<double, ImageDimension>;
  using CollidingFrontsFilterType2 = itk::CollidingFrontsImageFilter<DoubleImageType, InternalImageType>;
  auto collidingFronts2 = CollidingFrontsFilterType2::New();

  return EXIT_SUCCESS;
}
