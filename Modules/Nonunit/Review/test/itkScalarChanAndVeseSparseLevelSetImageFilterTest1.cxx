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

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"
#include "itkTestingMacros.h"

int
itkScalarChanAndVeseSparseLevelSetImageFilterTest1(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;
  using OutputImageType = ImageType;

  using DataHelperType = itk::ScalarChanAndVeseLevelSetFunctionData<ImageType, FeatureImageType>;

  using SharedDataHelperType =
    itk::ConstrainedRegionBasedLevelSetFunctionSharedData<ImageType, FeatureImageType, DataHelperType>;

  using RegionBasedLevelSetFunctionType =
    itk::ScalarChanAndVeseLevelSetFunction<ImageType, FeatureImageType, SharedDataHelperType>;

  auto function = RegionBasedLevelSetFunctionType::New();
  if (function.IsNull())
  {
    return EXIT_FAILURE;
  }

  using FilterType = itk::ScalarChanAndVeseSparseLevelSetImageFilter<ImageType,
                                                                     FeatureImageType,
                                                                     OutputImageType,
                                                                     RegionBasedLevelSetFunctionType,
                                                                     SharedDataHelperType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    filter, ScalarChanAndVeseSparseLevelSetImageFilter, MultiphaseSparseFiniteDifferenceImageFilter);


  std::cout << "Test finished. " << std::endl;
  return EXIT_SUCCESS;
}
