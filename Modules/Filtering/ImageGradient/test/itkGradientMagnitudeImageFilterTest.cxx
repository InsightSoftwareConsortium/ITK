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

#include <iostream>
#include "itkGradientMagnitudeImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"


inline std::ostream &
operator<<(std::ostream & o, const itk::Vector<float, 3> & v)
{
  o << "[" << v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int
itkGradientMagnitudeImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " useImageSpacing" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, Dimension>;

  // Set up filter
  itk::GradientMagnitudeImageFilter<ImageType, ImageType>::Pointer filter =
    itk::GradientMagnitudeImageFilter<ImageType, ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GradientMagnitudeImageFilter, ImageToImageFilter);


  auto useImageSpacing = static_cast<bool>(std::stoi(argv[1]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter->SetUseImageSpacingOn();
  }
  else
  {
    filter->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

  // Run test
  itk::Size<Dimension> sz;
  sz[0] = 100;
  sz[1] = 100;
  itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
