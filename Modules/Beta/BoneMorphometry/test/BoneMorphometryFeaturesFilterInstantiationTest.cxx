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
#include "itkBoneMorphometryFeaturesFilter.h"

#include "itkMath.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int
BoneMorphometryFeaturesFilterInstantiationTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " maskImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 3;

  // Declare types
  using InputPixelType = float;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  using FilterType = itk::BoneMorphometryFeaturesFilter<InputImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BoneMorphometryFeaturesFilter, ImageToImageFilter);

  filter->SetInput(reader->GetOutput());

  // filter->SetMaskImage( maskReader->GetOutput() );
  // ITK_TEST_SET_GET_VALUE( maskReader->GetOutput(), filter->GetMaskImage() );

  filter->SetThreshold(1300);
  ITK_TEST_SET_GET_VALUE(1300, filter->GetThreshold());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.232113, filter->GetBVTV(), 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.281487, filter->GetTbN(), 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.824595, filter->GetTbTh(), 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(2.72796, filter->GetTbSp(), 5, 0.00001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(2.42543, filter->GetBSBV(), 5, 0.00001));

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
