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


#include "itkThresholdLabelerImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkTestingMacros.h"

int
ThresholdLabelerImageFilterTestHelper(bool useRealTypeThresholds)
{
  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = float;
  using LabeledPixelType = unsigned long;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using LabeledImageType = itk::Image<LabeledPixelType, Dimension>;

  // Create an image with stripes to label
  InputImageType::IndexType  index = { { 0, 0 } };
  InputImageType::SizeType   size = { { 32, 32 } };
  InputImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  InputImageType::Pointer inputImage = InputImageType::New();
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->Allocate();

  size[0] = 32;
  size[1] = 8;
  region.SetSize(size);

  // Stripe y indexes
  using IndexValueVectorType = std::vector<InputImageType::IndexType::IndexValueType>;
  IndexValueVectorType yindexes;
  yindexes.push_back(0);
  yindexes.push_back(8);
  yindexes.push_back(16);
  yindexes.push_back(24);

  // Set the values for each stripe
  std::vector<InputPixelType> values;
  values.push_back(0.5);
  values.push_back(1.5);
  values.push_back(2.5);
  values.push_back(3.5);

  // Set the value for the offset
  unsigned long offset = 4;

  //  Set the labels vector
  std::vector<LabeledPixelType> labels;
  labels.push_back(0 + offset);
  labels.push_back(1 + offset);
  labels.push_back(2 + offset);
  labels.push_back(3 + offset);

  // Fill in the image
  unsigned int                         i;
  IndexValueVectorType::const_iterator indexIter;
  for (indexIter = yindexes.begin(), i = 0; indexIter != yindexes.end(); ++indexIter, ++i)
  {
    index[0] = 0;
    index[1] = *indexIter;
    region.SetIndex(index);
    itk::ImageRegionIterator<InputImageType> iter(inputImage, region);
    for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
      iter.Set(values[i]);
    }
  }

  // Apply labeler filter
  using LabelerFilterType = itk::ThresholdLabelerImageFilter<InputImageType, LabeledImageType>;
  LabelerFilterType::Pointer labelerFilter = LabelerFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(labelerFilter, ThresholdLabelerImageFilter, UnaryFunctorImageFilter);

  labelerFilter->SetInput(inputImage);

  if (!useRealTypeThresholds)
  {
    // Set the thresholds between values
    std::vector<InputPixelType> thresholds;
    thresholds.push_back(1.0);
    thresholds.push_back(2.0);
    thresholds.push_back(3.0);

    labelerFilter->SetThresholds(thresholds);
    // ITK_TEST_SET_GET_VALUE( thresholds, labelerFilter->GetThresholds() );
  }
  else
  {
    // Set the thresholds between values
    std::vector<LabelerFilterType::RealThresholdType> thresholds;
    thresholds.push_back(1.0);
    thresholds.push_back(2.0);
    thresholds.push_back(3.0);

    labelerFilter->SetRealThresholds(thresholds);
    // ITK_TEST_SET_GET_VALUE( thresholds, labelerFilter->GetRealThresholds() );
  }

  labelerFilter->SetLabelOffset(offset);
  ITK_TEST_SET_GET_VALUE(offset, labelerFilter->GetLabelOffset());


  ITK_TRY_EXPECT_NO_EXCEPTION(labelerFilter->SetFunctor(labelerFilter->GetFunctor()));


  ITK_TRY_EXPECT_NO_EXCEPTION(labelerFilter->Update());


  // Check if labels coincide with expected labels
  bool passed = true;

  for (indexIter = yindexes.begin(), i = 0; indexIter != yindexes.end(); ++indexIter, ++i)
  {
    index[0] = 0;
    index[1] = *indexIter;
    region.SetIndex(index);
    itk::ImageRegionConstIterator<LabeledImageType> iter(labelerFilter->GetOutput(), region);
    for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
      if (iter.Get() != labels[i])
      {
        passed = false;
        break;
      }
    }
    if (!passed)
    {
      break;
    }
  }

  if (!passed)
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << labelerFilter << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkThresholdLabelerImageFilterTest(int, char *[])
{
  bool testStatus = EXIT_SUCCESS;
  bool testStatusRealTypeThresholds = EXIT_SUCCESS;
  bool testStatusNotRealTypeThresholds = EXIT_SUCCESS;

  bool useRealTypeThresholds = true;
  testStatusRealTypeThresholds = ThresholdLabelerImageFilterTestHelper(useRealTypeThresholds);

  useRealTypeThresholds = false;
  testStatusNotRealTypeThresholds = ThresholdLabelerImageFilterTestHelper(useRealTypeThresholds);

  if (testStatusRealTypeThresholds == EXIT_SUCCESS && testStatusNotRealTypeThresholds == EXIT_SUCCESS)
  {
    std::cout << "Test finished." << std::endl;
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    testStatus = EXIT_FAILURE;
  }

  return testStatus;
}
