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

#include "itkLabelVotingImageFilter.h"
#include "itkTestingMacros.h"


int
itkLabelVotingImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Declare the pixel types of the images
  using PixelType = unsigned int;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  // Input data arrays for test images
  const unsigned int dataImageA[8] = { 0, 1, 3, 3, 4, 6, 6, 0 };
  const unsigned int dataImageB[8] = { 1, 1, 2, 4, 4, 5, 7, 1 };
  const unsigned int dataImageC[8] = { 0, 2, 2, 3, 5, 5, 6, 8 };

  // Correct combinations of input images
  const unsigned int combinationABC[8] = { 0, 1, 2, 3, 4, 5, 6, 9 };
  const unsigned int combinationAB[8] = { 8, 1, 8, 8, 4, 8, 8, 8 };
  const unsigned int combinationABundecided255[8] = { 255, 1, 255, 255, 4, 255, 255, 255 };

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Declare appropriate Iterator type for the images
  using IteratorType = itk::ImageRegionIterator<ImageType>;

  // Declare the type for the filter
  using LabelVotingImageFilterType = itk::LabelVotingImageFilter<ImageType>;

  // Create the input images
  auto inputImageA = ImageType::New();
  auto inputImageB = ImageType::New();
  auto inputImageC = ImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  IteratorType it = IteratorType(inputImageA, inputImageA->GetBufferedRegion());

  for (int i = 0; i < 8; ++i, ++it)
  {
    it.Set(dataImageA[i]);
  }

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  it = IteratorType(inputImageB, inputImageB->GetBufferedRegion());
  for (int i = 0; i < 8; ++i, ++it)
  {
    it.Set(dataImageB[i]);
  }

  // Initialize Image C
  inputImageC->SetRegions(region);
  inputImageC->Allocate();

  it = IteratorType(inputImageC, inputImageC->GetBufferedRegion());
  for (int i = 0; i < 8; ++i, ++it)
  {
    it.Set(dataImageC[i]);
  }

  // Create the LabelVoting Filter
  auto labelVotingFilter = LabelVotingImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(labelVotingFilter, LabelVotingImageFilter, ImageToImageFilter);


  // Test with first two input images with undecided label set to 255
  //

  // Set the first two input images
  labelVotingFilter->SetInput(0, inputImageA);
  labelVotingFilter->SetInput(1, inputImageB);

  // Set label for undecided pixels
  labelVotingFilter->SetLabelForUndecidedPixels(255);

  // Execute the filter
  labelVotingFilter->Update();

  // Get the filter output
  ImageType::Pointer outputImage = labelVotingFilter->GetOutput();

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < 8; ++i, ++it)
  {
    if (combinationABundecided255[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B and undecided=255: "
                << "i = " << i << ", Expected = " << combinationABundecided255[i] << ", Received = " << it.Get()
                << "\n";
      return EXIT_FAILURE;
    }
  }


  // Test with first two input images
  //

  // unset undecided pixel label; reinstate automatic selection
  labelVotingFilter->UnsetLabelForUndecidedPixels();

  // Execute the filter
  labelVotingFilter->Update();

  // Get the filter output
  outputImage = labelVotingFilter->GetOutput();

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < 8; ++i, ++it)
  {
    if (combinationAB[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B: i = " << i << ", Expected = " << combinationAB[i]
                << ", Received = " << it.Get() << "\n";
      return EXIT_FAILURE;
    }
  }


  // Test with all three input images
  //

  // Set the third input image
  labelVotingFilter->SetInput(2, inputImageC);

  // Execute the filter
  labelVotingFilter->Update();

  // Get the filter output
  outputImage = labelVotingFilter->GetOutput();

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < 8; ++i, ++it)
  {
    if (combinationABC[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B,C: i = " << i << ", Expected = " << combinationABC[i]
                << ", Received = " << it.Get() << "\n";
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test succeeded." << std::endl;

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
