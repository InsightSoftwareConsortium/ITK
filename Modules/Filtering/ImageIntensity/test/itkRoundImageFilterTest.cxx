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

#include "itkImageRegionIterator.h"
#include "itkMath.h"
#include "itkRandomImageSource.h"
#include "itkRoundImageFilter.h"
#include "itkTestingMacros.h"


int
itkRoundImageFilterTest(int, char *[])
{

  constexpr unsigned int Dimension = 2;

  using InputPixelType = float;
  using OutputPixelType = int;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ImageSourceType = itk::RandomImageSource<InputImageType>;

  using FilterType = itk::RoundImageFilter<InputImageType, OutputImageType>;
  auto roundImageFilter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(roundImageFilter, RoundImageFilter, UnaryGeneratorImageFilter);


  // Create a random image
  auto randomImageSource = ImageSourceType::New();

  const InputImageType::SizeType size{ { 10, 10 } };
  randomImageSource->SetSize(size);

  randomImageSource->SetMin(0.0);
  randomImageSource->SetMax(10.0);

  randomImageSource->SetNumberOfWorkUnits(1);

  roundImageFilter->SetInput(randomImageSource->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(roundImageFilter->Update());

  itk::ImageRegionIterator<InputImageType>  it1(randomImageSource->GetOutput(),
                                               randomImageSource->GetOutput()->GetLargestPossibleRegion());
  itk::ImageRegionIterator<OutputImageType> it2(roundImageFilter->GetOutput(),
                                                roundImageFilter->GetOutput()->GetLargestPossibleRegion());

  it1.GoToBegin();
  it2.GoToBegin();

  while (!it1.IsAtEnd() || !it2.IsAtEnd())
  {
    ITK_TEST_EXPECT_EQUAL(itk::Math::Round<OutputImageType::ValueType>(it1.Get()), it2.Get());

    ++it1;
    ++it2;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
