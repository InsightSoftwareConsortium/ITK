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

#include "itkAnchorOpenCloseImageFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkTestingMacros.h"


int
itkAnchorOpenCloseImageFilterTest(int, char ** const)
{
  constexpr unsigned int Dimension = 2;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, Dimension>;
  using KernelType = itk::FlatStructuringElement<Dimension>;
  using CompateType1 = std::less<typename ImageType::PixelType>;
  using CompateType2 = std::greater<typename ImageType::PixelType>;

  using FilterType = itk::AnchorOpenCloseImageFilter<ImageType, KernelType, CompateType1, CompateType2>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, AnchorOpenCloseImageFilter, KernelImageFilter);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
