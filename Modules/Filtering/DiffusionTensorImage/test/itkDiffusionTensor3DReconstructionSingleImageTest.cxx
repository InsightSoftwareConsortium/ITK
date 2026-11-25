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

// Exercises the single-VectorImage gradient path (SetGradientImage), which the
// existing reconstruction test does not cover, and verifies it produces the same
// tensors as the multi-image path (AddGradientImage) on identical data.

#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkImageRegionIterator.h"
#include <cmath>
#include "itkTestingMacros.h"

int
itkDiffusionTensor3DReconstructionSingleImageTest(int, char *[])
{
  using ReferencePixelType = short;
  using GradientPixelType = short;
  using TensorPrecisionType = double;
  using FilterType =
    itk::DiffusionTensor3DReconstructionImageFilter<ReferencePixelType, GradientPixelType, TensorPrecisionType>;

  constexpr unsigned int numberOfGradientImages = 6;
  const double           gradientDirections[numberOfGradientImages][3] = {
    { -1.000000, 0.000000, 0.000000 },   { -0.166000, 0.986000, 0.000000 }, { 0.110000, 0.664000, 0.740000 },
    { -0.901000, -0.419000, -0.110000 }, { 0.169000, -0.601000, 0.781000 }, { 0.815000, -0.386000, 0.433000 }
  };
  constexpr ReferencePixelType b0 = 100;
  auto gradientValue = [](unsigned int i) { return static_cast<GradientPixelType>((i + 1) * (i + 1) * (i + 1)); };

  using ReferenceImageType = FilterType::ReferenceImageType;
  using RegionType = ReferenceImageType::RegionType;
  constexpr RegionType::SizeType imageSize{ 4, 4, 4 };
  const RegionType               region{ imageSize };

  // --- Path A: multi-image (AddGradientImage) ---
  auto multiFilter = FilterType::New();
  {
    auto reference = ReferenceImageType::New();
    reference->SetRegions(region);
    reference->Allocate();
    reference->FillBuffer(b0);
    multiFilter->SetReferenceImage(reference);
    for (unsigned int i = 0; i < numberOfGradientImages; ++i)
    {
      using GradientImageType = FilterType::GradientImageType;
      auto gradientImage = GradientImageType::New();
      gradientImage->SetRegions(region);
      gradientImage->Allocate();
      gradientImage->FillBuffer(gradientValue(i));
      FilterType::GradientDirectionType d;
      d[0] = gradientDirections[i][0];
      d[1] = gradientDirections[i][1];
      d[2] = gradientDirections[i][2];
      multiFilter->AddGradientImage(d, gradientImage);
    }
  }
  multiFilter->SetBValue(1000.0);
  multiFilter->Update();

  // --- Path B: single VectorImage (SetGradientImage) with the SAME data ---
  // Component 0 is the b0 baseline (direction (0,0,0)); components 1..n are the
  // diffusion-weighted measurements.
  auto singleFilter = FilterType::New();
  {
    using VectorImageType = itk::VectorImage<GradientPixelType, 3>;
    auto vectorImage = VectorImageType::New();
    vectorImage->SetRegions(region);
    vectorImage->SetVectorLength(numberOfGradientImages + 1);
    vectorImage->Allocate();
    itk::VariableLengthVector<GradientPixelType> value(numberOfGradientImages + 1);
    value[0] = b0;
    for (unsigned int i = 0; i < numberOfGradientImages; ++i)
    {
      value[i + 1] = gradientValue(i);
    }
    vectorImage->FillBuffer(value);

    auto                              directions = FilterType::GradientDirectionContainerType::New();
    FilterType::GradientDirectionType zero{};
    zero.fill(0.0);
    directions->InsertElement(0, zero);
    for (unsigned int i = 0; i < numberOfGradientImages; ++i)
    {
      FilterType::GradientDirectionType d;
      d[0] = gradientDirections[i][0];
      d[1] = gradientDirections[i][1];
      d[2] = gradientDirections[i][2];
      directions->InsertElement(i + 1, d);
    }
    singleFilter->SetGradientImage(directions, vectorImage);
  }
  singleFilter->SetBValue(1000.0);
  singleFilter->Update();

  // --- The two paths must reconstruct identical tensors ---
  using TensorImageType = FilterType::TensorImageType;
  itk::ImageRegionConstIterator<TensorImageType> mit(multiFilter->GetOutput(),
                                                     multiFilter->GetOutput()->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<TensorImageType> sit(singleFilter->GetOutput(),
                                                     singleFilter->GetOutput()->GetLargestPossibleRegion());
  double                                         maxDiff = 0.0;
  constexpr double                               tolerance = 1e-8;
  bool                                           anyNonZero = false;
  for (mit.GoToBegin(), sit.GoToBegin(); !mit.IsAtEnd(); ++mit, ++sit)
  {
    for (unsigned int k = 0; k < 6; ++k)
    {
      maxDiff = std::max(maxDiff, std::abs(mit.Get()[k] - sit.Get()[k]));
      anyNonZero = anyNonZero || (std::abs(sit.Get()[k]) > 1e-12);
    }
  }

  std::cout << "SingleImage vs MultiImage max tensor difference: " << maxDiff << std::endl;
  ITK_TEST_EXPECT_TRUE(anyNonZero);
  if (maxDiff > tolerance)
  {
    std::cerr << "Single-image path disagrees with multi-image path (max diff " << maxDiff << " > " << tolerance << ')'
              << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
