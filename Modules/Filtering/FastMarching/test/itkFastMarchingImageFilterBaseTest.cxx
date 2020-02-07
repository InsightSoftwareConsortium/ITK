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

#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkTestingMacros.h"

/*
 * This function ONLY tests basic getters/setters for the base class
 * and does not exercise the base class filter. The process of testing
 * the running of the filter is delegated to other tests.
 * @tparam VDimension
 * @return EXIT_SUCCESS if all test passs, EXIT_FAILURE otherwise
 */
template <unsigned int VDimension>
static int
FastMarchingImageFilterBaseTestFunction()
{
  using PixelType = float;

  using ImageType = itk::Image<PixelType, VDimension>;
  typename ImageType::Pointer input = ImageType::New();

  using FastMarchingImageFilterType = itk::FastMarchingImageFilterBase<ImageType, ImageType>;
  typename FastMarchingImageFilterType::Pointer fastMarchingFilter = FastMarchingImageFilterType::New();

  bool overrideOutputInformation = true;
  ITK_TEST_SET_GET_BOOLEAN(fastMarchingFilter, OverrideOutputInformation, overrideOutputInformation);

  typename FastMarchingImageFilterType::OutputSizeType outputSize;
  outputSize.Fill(32);
  fastMarchingFilter->SetOutputSize(outputSize);
  ITK_TEST_SET_GET_VALUE(outputSize, fastMarchingFilter->GetOutputSize());

  typename FastMarchingImageFilterType::OutputRegionType outputRegion;
  outputRegion.SetSize(outputSize);
  fastMarchingFilter->SetOutputRegion(outputRegion);
  ITK_TEST_SET_GET_VALUE(outputRegion, fastMarchingFilter->GetOutputRegion());

  typename FastMarchingImageFilterType::OutputSpacingType outputSpacing;
  outputSpacing.Fill(1.0);
  fastMarchingFilter->SetOutputSpacing(outputSpacing);
  ITK_TEST_SET_GET_VALUE(outputSpacing, fastMarchingFilter->GetOutputSpacing());

  typename FastMarchingImageFilterType::OutputDirectionType outputDirection;
  outputDirection.SetIdentity();
  fastMarchingFilter->SetOutputDirection(outputDirection);
  ITK_TEST_SET_GET_VALUE(outputDirection, fastMarchingFilter->GetOutputDirection());

  typename FastMarchingImageFilterType::OutputPointType outputOrigin;
  outputOrigin.Fill(0.0);
  fastMarchingFilter->SetOutputOrigin(outputOrigin);
  ITK_TEST_SET_GET_VALUE(outputOrigin, fastMarchingFilter->GetOutputOrigin());

  // DO NOT ATTEMPT TO UPDATE the base class filter.
  // the base class filter is not sufficiently configured
  // with trial point, stopping criteria, normalization factors, or speed constants.
  // or given a valid image.
  //
  // DO NOT ADD ITK_TRY_EXPECT_NO_EXCEPTION( fastMarchingFilter->Update() );
  // DO NOT ADD typename ImageType::Pointer output = fastMarchingFilter->GetOutput();

  return EXIT_SUCCESS;
}


int
itkFastMarchingImageFilterBaseTest(int, char *[])
{
  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  const unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using FastMarchingImageFilterType = itk::FastMarchingImageFilterBase<ImageType, ImageType>;

  FastMarchingImageFilterType::Pointer fastMarchingFilter = FastMarchingImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(fastMarchingFilter, FastMarchingImageFilterBase, FastMarchingBase);


  if (FastMarchingImageFilterBaseTestFunction<2>() == EXIT_FAILURE)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }
  if (FastMarchingImageFilterBaseTestFunction<3>() == EXIT_FAILURE)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
