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

#include "itkRandomImageSource.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkVotingBinaryIterativeHoleFillingImageFilter.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"


int
itkVotingBinaryIterativeHoleFillingImageFilterTest(int, char *[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());


  using ImageType = itk::Image<unsigned short, 2>;

  itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  random->SetMin(0);
  random->SetMax(100);

  ImageType::SizeValueType randomSize[2];
  randomSize[0] = randomSize[1] = 8;
  random->SetSize(randomSize);

  ImageType::SpacingValueType spacing[2] = { 0.7, 2.1 };
  random->SetSpacing(spacing);

  ImageType::PointValueType origin[2] = { 15, 400 };
  random->SetOrigin(origin);

  ImageType::PixelType foreground = 97; // prime numbers are good testers
  ImageType::PixelType background = 29;

  itk::BinaryThresholdImageFilter<ImageType, ImageType>::Pointer thresholder;
  thresholder = itk::BinaryThresholdImageFilter<ImageType, ImageType>::New();
  thresholder->SetInput(random->GetOutput());
  thresholder->SetLowerThreshold(30);
  thresholder->SetUpperThreshold(100);
  thresholder->SetInsideValue(foreground);
  thresholder->SetOutsideValue(background);

  // Create a voting image
  itk::VotingBinaryIterativeHoleFillingImageFilter<ImageType>::Pointer voting;
  voting = itk::VotingBinaryIterativeHoleFillingImageFilter<ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(voting, VotingBinaryIterativeHoleFillingImageFilter, ImageToImageFilter);


  voting->SetForegroundValue(foreground);
  ITK_TEST_SET_GET_VALUE(foreground, voting->GetForegroundValue());

  voting->SetBackgroundValue(background);
  ITK_TEST_SET_GET_VALUE(background, voting->GetBackgroundValue());

  // define the neighborhood size used for the voting filter (5x5)
  ImageType::SizeType neighRadius;
  neighRadius[0] = 1;
  neighRadius[1] = 1;
  voting->SetRadius(neighRadius);
  ITK_TEST_SET_GET_VALUE(neighRadius, voting->GetRadius());

  // Set the maximum number of times the filter should perform passes filling
  // the border of holes and cavities.
  unsigned int maximumNumberOfIterations = 10;
  voting->SetMaximumNumberOfIterations(maximumNumberOfIterations);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfIterations, voting->GetMaximumNumberOfIterations());

  unsigned int currentNumberOfIterations = 0;
  voting->SetCurrentNumberOfIterations(currentNumberOfIterations);
  ITK_TEST_SET_GET_VALUE(currentNumberOfIterations, voting->GetCurrentNumberOfIterations());

  // Set the number of pixels over 50% that will tip the decision about
  // switching a pixel.
  unsigned int majorityThreshold = 1;
  voting->SetMajorityThreshold(majorityThreshold);
  ITK_TEST_SET_GET_VALUE(majorityThreshold, voting->GetMajorityThreshold());


  voting->SetInput(thresholder->GetOutput());

  // run the algorithm
  voting->Update();

  itk::ImageRegionIterator<ImageType> it;
  it = itk::ImageRegionIterator<ImageType>(random->GetOutput(), random->GetOutput()->GetBufferedRegion());
  std::cout << "Input image" << std::endl;
  unsigned int i;
  for (i = 1; !it.IsAtEnd(); ++i, ++it)
  {
    std::cout << "\t" << it.Get();
    if ((i % 8) == 0)
    {
      std::cout << std::endl;
    }
  }

  it = itk::ImageRegionIterator<ImageType>(thresholder->GetOutput(), thresholder->GetOutput()->GetBufferedRegion());
  std::cout << "Binary image" << std::endl;

  for (i = 1; !it.IsAtEnd(); ++i, ++it)
  {
    std::cout << "\t" << it.Get();
    if ((i % 8) == 0)
    {
      std::cout << std::endl;
    }
  }


  std::cout << "Output image" << std::endl;
  it = itk::ImageRegionIterator<ImageType>(voting->GetOutput(), voting->GetOutput()->GetBufferedRegion());
  for (i = 1; !it.IsAtEnd(); ++i, ++it)
  {
    std::cout << "\t" << it.Get();
    if ((i % 8) == 0)
    {
      std::cout << std::endl;
    }
  }

  std::cout << "Number Of Pixels Changed = " << voting->GetNumberOfPixelsChanged() << std::endl;

  voting->Print(std::cout);
  return EXIT_SUCCESS;
}
