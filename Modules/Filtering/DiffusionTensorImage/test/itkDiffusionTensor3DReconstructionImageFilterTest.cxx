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
#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include <iostream>

int
itkDiffusionTensor3DReconstructionImageFilterTest(int argc, char * argv[])
{
  // Check parameters
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " bValue" << std::endl;
    return EXIT_FAILURE;
  }

  using ReferencePixelType = short;
  using GradientPixelType = short;
  using TensorPrecisionType = double;

  int result(EXIT_SUCCESS);

  for (unsigned int pass = 0; pass < 2; ++pass)
  {
    using TensorReconstructionImageFilterType =
      itk::DiffusionTensor3DReconstructionImageFilter<ReferencePixelType, GradientPixelType, TensorPrecisionType>;
    using GradientImageType = TensorReconstructionImageFilterType::GradientImageType;
    TensorReconstructionImageFilterType::Pointer tensorReconstructionFilter =
      TensorReconstructionImageFilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      tensorReconstructionFilter, DiffusionTensor3DReconstructionImageFilter, ImageToImageFilter);

    auto threshold = itk::NumericTraits<TensorReconstructionImageFilterType::ReferencePixelType>::min();
    tensorReconstructionFilter->SetThreshold(threshold);
    ITK_TEST_SET_GET_VALUE(threshold, tensorReconstructionFilter->GetThreshold());

    auto bValue = static_cast<TensorPrecisionType>(std::stod(argv[1]));
    tensorReconstructionFilter->SetBValue(bValue);
    ITK_TEST_SET_GET_VALUE(bValue, tensorReconstructionFilter->GetBValue());

    // Create a reference image
    //
    using ReferenceImageType = TensorReconstructionImageFilterType::ReferenceImageType;
    auto referenceImage = ReferenceImageType::New();
    using ReferenceRegionType = ReferenceImageType::RegionType;
    using ReferenceIndexType = ReferenceRegionType::IndexType;
    using ReferenceSizeType = ReferenceRegionType::SizeType;
    ReferenceSizeType   sizeReferenceImage = { { 4, 4, 4 } };
    ReferenceIndexType  indexReferenceImage = { { 0, 0, 0 } };
    ReferenceRegionType regionReferenceImage;
    regionReferenceImage.SetSize(sizeReferenceImage);
    regionReferenceImage.SetIndex(indexReferenceImage);
    referenceImage->SetRegions(regionReferenceImage);
    referenceImage->Allocate();
    referenceImage->FillBuffer(100);


    constexpr unsigned int numberOfGradientImages = 6;

    // Assign gradient directions
    //
    double gradientDirections[6][3] = { { -1.000000, 0.000000, 0.000000 }, { -0.166000, 0.986000, 0.000000 },
                                        { 0.110000, 0.664000, 0.740000 },  { -0.901000, -0.419000, -0.110000 },
                                        { 0.169000, -0.601000, 0.781000 }, { 0.815000, -0.386000, 0.433000 } };


    // Create gradient images
    //
    using GradientImageType = TensorReconstructionImageFilterType::GradientImageType;
    using GradientRegionType = GradientImageType::RegionType;
    using GradientIndexType = GradientRegionType::IndexType;
    using GradientSizeType = GradientRegionType::SizeType;
    using ReferenceIndexType = ReferenceRegionType::IndexType;

    for (unsigned int i = 0; i < numberOfGradientImages; ++i)
    {
      auto               gradientImage = GradientImageType::New();
      GradientSizeType   sizeGradientImage = { { 4, 4, 4 } };
      GradientIndexType  indexGradientImage = { { 0, 0, 0 } };
      GradientRegionType regionGradientImage;
      regionGradientImage.SetSize(sizeGradientImage);
      regionGradientImage.SetIndex(indexGradientImage);
      gradientImage->SetRegions(regionGradientImage);
      gradientImage->Allocate();

      itk::ImageRegionIteratorWithIndex<GradientImageType> git(gradientImage, regionGradientImage);
      git.GoToBegin();
      while (!git.IsAtEnd())
      {
        auto fancyGradientValue = static_cast<short>((i + 1) * (i + 1) * (i + 1));
        git.Set(fancyGradientValue);
        ++git;
      }

      TensorReconstructionImageFilterType::GradientDirectionType gradientDirection;
      gradientDirection[0] = gradientDirections[i][0];
      gradientDirection[1] = gradientDirections[i][1];
      gradientDirection[2] = gradientDirections[i][2];
      tensorReconstructionFilter->AddGradientImage(gradientDirection, gradientImage);
      std::cout << "Gradient directions: " << gradientDirection << std::endl;

      const TensorReconstructionImageFilterType::GradientDirectionType::element_type epsilon = 1e-3;
      TensorReconstructionImageFilterType::GradientDirectionType                     output =
        tensorReconstructionFilter->GetGradientDirection(i);
      for (unsigned int j = 0; j < gradientDirection.size(); ++j)
      {
        TensorReconstructionImageFilterType::GradientDirectionType::element_type gradientDirectionComponent =
          gradientDirection[j];
        TensorReconstructionImageFilterType::GradientDirectionType::element_type outputComponent = output[j];
        if (!itk::Math::FloatAlmostEqual(gradientDirectionComponent, outputComponent, 10, epsilon))
        {
          std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
          std::cerr << "Test failed!" << std::endl;
          std::cerr << "Error in gradientDirection [" << i << "]"
                    << "[" << j << "]" << std::endl;
          std::cerr << "Expected value " << gradientDirectionComponent << std::endl;
          std::cerr << " differs from " << outputComponent;
          std::cerr << " by more than " << epsilon << std::endl;
          return EXIT_FAILURE;
        }
      }
    }

    // Test gradient direction index exception
    unsigned int idx = numberOfGradientImages + 1;
    ITK_TRY_EXPECT_EXCEPTION(tensorReconstructionFilter->GetGradientDirection(idx));

    //
    // second time through test, use the mask image
    if (pass == 1)
    {
      // Create a mask image
      // With all pixels set to 255, it won't actually suppress any voxels
      // from processing, but it will at least exercise the mask code.
      using MaskImageType = TensorReconstructionImageFilterType::MaskImageType;
      auto maskImage = MaskImageType::New();
      maskImage->SetRegions(regionReferenceImage);
      maskImage->Allocate();
      maskImage->FillBuffer(255);
      tensorReconstructionFilter->SetMaskImage(maskImage);
      //
      // just for coverage, use the Spatial Object input type as well.
      itk::ImageMaskSpatialObject<3>::Pointer maskSpatialObject = itk::ImageMaskSpatialObject<3>::New();
      maskSpatialObject->SetImage(maskImage);
      maskSpatialObject->Update();
      tensorReconstructionFilter->SetMaskSpatialObject(maskSpatialObject);
    }
    tensorReconstructionFilter->SetReferenceImage(referenceImage);
    ITK_TEST_SET_GET_VALUE(referenceImage, tensorReconstructionFilter->GetReferenceImage());

    // TODO: remove this when netlib is made thread safe
    tensorReconstructionFilter->SetNumberOfWorkUnits(1);

    // Also see if vnl_svd is thread safe now...
    std::cout << std::endl
              << "This filter is using " << tensorReconstructionFilter->GetNumberOfWorkUnits() << " work units "
              << std::endl;

    itk::SimpleFilterWatcher watcher(tensorReconstructionFilter, "Tensor Reconstruction");

    tensorReconstructionFilter->Update();

    using TensorImageType = TensorReconstructionImageFilterType::TensorImageType;
    TensorImageType::Pointer tensorImage = tensorReconstructionFilter->GetOutput();
    using TensorImageIndexType = TensorImageType::IndexType;

    TensorImageIndexType tensorImageIndex = { { 3, 3, 3 } };
    GradientIndexType    gradientImageIndex = { { 3, 3, 3 } };
    ReferenceIndexType   referenceImageIndex = { { 3, 3, 3 } };

    std::cout << std::endl << "Pixels at index: " << tensorImageIndex << std::endl;
    std::cout << "Reference pixel " << referenceImage->GetPixel(referenceImageIndex) << std::endl;

    for (unsigned int i = 0; i < numberOfGradientImages; ++i)
    {
      const GradientImageType * gradImage(tensorReconstructionFilter->GetGradientImage(i));
      std::cout << "Gradient image " << i << " pixel : " << gradImage->GetPixel(gradientImageIndex) << std::endl;
    }

    constexpr double expectedResult[3][3] = { { 4.60517, -2.6698, -8.4079 },
                                              { -2.6698, 1.56783, 0.900034 },
                                              { -8.4079, 0.900034, 2.62504 } };

    std::cout << std::endl << "Reconstructed tensor : " << std::endl;
    bool   passed = true;
    double precision = 0.0001;
    for (unsigned int i = 0; i < 3; ++i)
    {
      std::cout << "\t";
      for (unsigned int j = 0; j < 3; ++j)
      {
        std::cout << tensorImage->GetPixel(tensorImageIndex)(i, j) << " ";
        if ((itk::Math::abs(tensorImage->GetPixel(tensorImageIndex)(i, j) - expectedResult[i][j])) > precision)
        {
          passed = false;
        }
      }
      std::cout << std::endl;
    }

    if (!passed)
    {
      std::cout << "[FAILED]" << std::endl;

      std::cout << "Expected tensor : " << std::endl;
      for (const auto & i : expectedResult)
      {
        std::cout << "\t";
        for (double j : i)
        {
          std::cout << j << " ";
        }
        std::cout << std::endl;
      }
      result = EXIT_FAILURE;
      continue;
    }
    std::cout << "[PASSED]" << std::endl;
  }

  // Test streaming enumeration for DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat elements
  const std::set<itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat> allGradientImageFormat{
    itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInASingleImage,
    itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInManyImages,
    itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::Else
  };
  for (const auto & ee : allGradientImageFormat)
  {
    std::cout << "STREAMED ENUM VALUE DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat: " << ee
              << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return result;
}
