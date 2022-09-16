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
#include "itkMultiLabelSTAPLEImageFilter.h"
#include "itkTestingMacros.h"

int
itkMultiLabelSTAPLEImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;
  constexpr unsigned int imageSizePerDimension = 2;
  constexpr unsigned int imageSize = 8; // std::pow(imageSizePerDimension, Dimension);

  // Declare the types of the images
  using ImageType = itk::Image<unsigned int, Dimension>;

  // Input data arrays for test images
  const unsigned int dataImageA[imageSize] = { 0, 1, 3, 3, 4, 6, 6, 0 };
  const unsigned int dataImageB[imageSize] = { 1, 1, 2, 4, 4, 5, 7, 1 };
  const unsigned int dataImageC[imageSize] = { 0, 2, 2, 3, 5, 5, 6, 8 };

  // Correct combinations of input images
  const unsigned int combinationABC[imageSize] = { 0, 1, 2, 3, 4, 5, 6, 9 };
  const unsigned int combinationAB[imageSize] = { 8, 1, 8, 8, 4, 8, 8, 8 };
  const unsigned int combinationABundecided255[imageSize] = { 255, 1, 255, 255, 4, 255, 255, 255 };

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Declare Iterator type appropriate for image
  using IteratorType = itk::ImageRegionIterator<ImageType>;

  // Declare the type for the ADD filter
  using FilterType = itk::MultiLabelSTAPLEImageFilter<ImageType>;
  using FilterTypePointer = FilterType::Pointer;

  // Declare the pointers to images
  using ImageTypePointer = ImageType::Pointer;

  // Create two images
  ImageTypePointer inputImageA = ImageType::New();
  ImageTypePointer inputImageB = ImageType::New();
  ImageTypePointer inputImageC = ImageType::New();

  RegionType region;
  {
    // Define their size, and start index
    SizeType size;
    size[0] = imageSizePerDimension;
    size[1] = imageSizePerDimension;
    size[2] = imageSizePerDimension;

    IndexType start;
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;

    region.SetIndex(start);
    region.SetSize(size);
  }

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  IteratorType it = IteratorType(inputImageA, inputImageA->GetBufferedRegion());

  for (unsigned int i = 0; i < imageSize; ++i, ++it)
  {
    it.Set(dataImageA[i]);
  }

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  it = IteratorType(inputImageB, inputImageB->GetBufferedRegion());
  for (unsigned int i = 0; i < imageSize; ++i, ++it)
  {
    it.Set(dataImageB[i]);
  }

  // Initialize Image C
  inputImageC->SetRegions(region);
  inputImageC->Allocate();

  it = IteratorType(inputImageC, inputImageC->GetBufferedRegion());
  for (unsigned int i = 0; i < imageSize; ++i, ++it)
  {
    it.Set(dataImageC[i]);
  }

  // Create an LabelVoting Filter
  FilterTypePointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, MultiLabelSTAPLEImageFilter, ImageToImageFilter);

  // Get the Smart Pointer to the Filter Output
  ImageTypePointer outputImage = filter->GetOutput();

  // Test first two input images with undecided label set to 255

  // Connect the first two input images
  filter->SetInput(0, inputImageA);
  filter->SetInput(1, inputImageB);

  ITK_TEST_EXPECT_TRUE(!filter->GetHasMaximumNumberOfIterations());

  unsigned int maximumNumberOfIterations = 100;
  filter->SetMaximumNumberOfIterations(maximumNumberOfIterations);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfIterations, filter->GetMaximumNumberOfIterations());

  ITK_TEST_EXPECT_TRUE(filter->GetHasMaximumNumberOfIterations());

  ITK_TEST_EXPECT_TRUE(!filter->GetHasLabelForUndecidedPixels());

  filter->UnsetMaximumNumberOfIterations();

  typename FilterType::WeightsType terminationUpdateThreshold = 1e-5;
  filter->SetTerminationUpdateThreshold(terminationUpdateThreshold);
  ITK_TEST_SET_GET_VALUE(terminationUpdateThreshold, filter->GetTerminationUpdateThreshold());

  // Set label for undecided pixels
  typename FilterType::OutputPixelType labelForUndecidedPixels = 255;
  filter->SetLabelForUndecidedPixels(labelForUndecidedPixels);
  ITK_TEST_SET_GET_VALUE(labelForUndecidedPixels, filter->GetLabelForUndecidedPixels());

  ITK_TEST_EXPECT_TRUE(filter->GetHasLabelForUndecidedPixels());

  ITK_TEST_EXPECT_TRUE(!filter->GetHasPriorProbabilities());

  typename FilterType::PriorProbabilitiesType::ValueType priorProbabilitiesVal(0.0);
  typename FilterType::PriorProbabilitiesType            priorProbabilities(1);
  priorProbabilities.Fill(priorProbabilitiesVal);
  filter->SetPriorProbabilities(priorProbabilities);
  ITK_TEST_SET_GET_VALUE(priorProbabilities, filter->GetPriorProbabilities());

  ITK_TEST_EXPECT_TRUE(filter->GetHasPriorProbabilities());

  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->UnsetPriorProbabilities();

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  std::cout << "ElapsedNumberOfIterations: " << filter->GetElapsedNumberOfIterations() << std::endl;

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < imageSize; ++i, ++it)
  {
    if (combinationABundecided255[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B and undecided=" << labelForUndecidedPixels << ": "
                << "i = " << i << ", correct = " << combinationABundecided255[i] << ", got = " << it.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Test first two input images

  // Unset undecided pixel label; reinstate automatic selection
  filter->UnsetLabelForUndecidedPixels();

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < 8; ++i, ++it)
  {
    if (combinationAB[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B: i = " << i << ", correct = " << combinationAB[i]
                << ", got = " << it.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Test all three input images

  // Connect third input image
  filter->SetInput(2, inputImageC);

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Compare to correct results
  it = IteratorType(outputImage, outputImage->GetBufferedRegion());
  for (unsigned int i = 0; i < 8; ++i, ++it)
  {
    if (combinationABC[i] != it.Get())
    {
      std::cout << "Incorrect result using images A,B,C: i = " << i << ", correct = " << combinationABC[i]
                << ", got = " << it.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }


  std::cout << "Prior probabilities: " << filter->GetPriorProbabilities() << std::endl;
  std::cout << "Confusion matrix 0 " << std::endl << filter->GetConfusionMatrix(0) << std::endl;
  std::cout << "Confusion matrix 1 " << std::endl << filter->GetConfusionMatrix(1) << std::endl;
  std::cout << "Confusion matrix 2 " << std::endl << filter->GetConfusionMatrix(2) << std::endl;

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
