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
#include "itkVectorImage.h"
#include "itkGradientImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"

#include "itkPeriodicBoundaryCondition.h"

inline std::ostream &
operator<<(std::ostream & o, const itk::CovariantVector<float, 3> & v)
{
  o << "[" << v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int
itkGradientImageFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " useImageSpacing useImageDirection" << std::endl;
    return EXIT_FAILURE;
  }

  using InputImageType1 = itk::Image<unsigned short, 2>;
  using FilterType1 = itk::GradientImageFilter<InputImageType1, float, float>;
  using OutputImageType1 = FilterType1::OutputImageType;


  // Set up filter
  auto filter1 = FilterType1::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter1, GradientImageFilter, ImageToImageFilter);


  auto useImageSpacing = static_cast<bool>(std::stoi(argv[1]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter1->SetUseImageSpacingOn();
  }
  else
  {
    filter1->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter1, UseImageSpacing, useImageSpacing);

  auto useImageDirection = static_cast<bool>(std::stoi(argv[2]));
  ITK_TEST_SET_GET_BOOLEAN(filter1, UseImageDirection, useImageDirection);

  // Run test
  itk::Size<2> sz1;
  sz1[0] = 100;
  sz1[1] = 100;
  itk::NullImageToImageFilterDriver<InputImageType1, OutputImageType1> test1;
  test1.SetImageSize(sz1);
  test1.SetFilter(filter1);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());


  // Verify that we can run with VectorImages
  using InputImageType2 = itk::Image<float, 3>;
  using OutputImageType2 = itk::VectorImage<float, 3>;

  using FilterType2 = itk::GradientImageFilter<InputImageType2, float, float, OutputImageType2>;

  auto filter2 = FilterType2::New();

  using PeriodicBoundaryType = itk::PeriodicBoundaryCondition<InputImageType2>;
  // Test the OverrideBoundaryCondition setting;
  filter2->OverrideBoundaryCondition(new PeriodicBoundaryType);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter2, GradientImageFilter, ImageToImageFilter);


  ITK_TEST_SET_GET_BOOLEAN(filter2, UseImageSpacing, useImageSpacing);

  ITK_TEST_SET_GET_BOOLEAN(filter2, UseImageDirection, useImageDirection);

  // Run test
  itk::Size<3> sz2;
  sz2[0] = 25;
  sz2[1] = 25;
  sz2[2] = 25;
  itk::NullImageToImageFilterDriver<InputImageType2, OutputImageType2> test2;
  test2.SetImageSize(sz2);
  test2.SetFilter(filter2);

  ITK_TRY_EXPECT_NO_EXCEPTION(test2.Execute());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
