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

#include "itkImageToVideoFilter.h"

#include <string>

#include "itkImage.h"
#include "itkVideoStream.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkTestingMacros.h"

int
itkImageToVideoFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFile outputFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = float;
  const unsigned int Dimension = 3;

  using ImageType = itk::Image<PixelType, Dimension>;

  // Get 3D image to represent a temporal dataset of 2D frames
  const auto                        inputImage = itk::ReadImage<ImageType>(argv[1]);
  typename ImageType::DirectionType inputDirection;
  /* Set input image direction matrix to
   * 1  0 0
   * 0  0 1
   * 0 -1 0
   * with axis 0 representing time and others representing spatial dimensions
   */
  inputDirection(0, 0) = 1;
  inputDirection(1, 2) = 1;
  inputDirection(2, 1) = -1;
  inputImage->SetDirection(inputDirection);

  using VideoFilterType = itk::ImageToVideoFilter<ImageType>;
  auto videoFilter = VideoFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(videoFilter, ImageToVideoFilter, VideoSource);


  videoFilter->SetInput(inputImage);
  // Arbitrarily set 0th axis as temporal dimension to split frames
  itk::IndexValueType frameAxis = 0;
  videoFilter->SetFrameAxis(frameAxis);
  ITK_TEST_SET_GET_VALUE(frameAxis, videoFilter->GetFrameAxis());

  ITK_TRY_EXPECT_NO_EXCEPTION(videoFilter->Update());

  auto videoOutput = videoFilter->GetOutput();
  auto imageRegion = inputImage->GetLargestPossibleRegion();

  // Verify start frame and frame duration in output match size of designated temporal axis in input
  ITK_TEST_EXPECT_EQUAL(
    static_cast<itk::IndexValueType>(videoOutput->GetLargestPossibleTemporalRegion().GetFrameStart()),
    static_cast<itk::IndexValueType>(inputImage->GetLargestPossibleRegion().GetIndex(frameAxis)));
  ITK_TEST_EXPECT_EQUAL(
    static_cast<itk::IndexValueType>(videoOutput->GetLargestPossibleTemporalRegion().GetFrameDuration()),
    static_cast<itk::IndexValueType>(inputImage->GetLargestPossibleRegion().GetSize(frameAxis)));
  ITK_TEST_EXPECT_EQUAL(videoOutput->GetLargestPossibleTemporalRegion().GetRealStart().GetTimeInSeconds(),
                        inputImage->GetOrigin()[frameAxis]);
  ITK_TEST_EXPECT_EQUAL(videoOutput->GetLargestPossibleTemporalRegion().GetRealDuration().GetTimeInSeconds(),
                        inputImage->GetSpacing()[frameAxis] *
                          inputImage->GetLargestPossibleRegion().GetSize(frameAxis));

  // Verify spatial dimensions, spacing, and origin in output frames match size of non-temporal axes in input
  for (itk::IndexValueType idx = 1; idx < ImageType::ImageDimension; idx++)
  {
    ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(0)->GetLargestPossibleRegion().GetSize(idx - 1),
                          inputImage->GetLargestPossibleRegion().GetSize(idx));
    ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(0)->GetSpacing()[idx - 1], inputImage->GetSpacing()[idx]);
    ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(0)->GetOrigin()[idx - 1], inputImage->GetOrigin()[idx]);
  }

  // Verify spatial direction in output frames match input direction
  typename VideoFilterType::OutputFrameType::DirectionType outputDirection;
  outputDirection(0, 1) = 1;
  outputDirection(1, 0) = -1;
  for (auto frameIdx = videoOutput->GetLargestPossibleTemporalRegion().GetFrameStart();
       frameIdx < videoOutput->GetLargestPossibleTemporalRegion().GetFrameDuration();
       ++frameIdx)
  {
    ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(frameIdx)->GetDirection(), outputDirection);
  }

  // Iterate over 3D input + video output to verify pixel data matches across each slice/frame
  using ImageIteratorType = typename itk::ImageSliceConstIteratorWithIndex<ImageType>;
  ImageIteratorType it(inputImage, inputImage->GetLargestPossibleRegion());
  it.SetFirstDirection(1);  // fastest moving remaining spatial axis
  it.SetSecondDirection(2); // second-fastest moving remaining spatial axis
  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfSlice())
    {
      while (!it.IsAtEndOfLine())
      {
        auto idx = it.GetIndex();
        auto frame = videoOutput->GetFrame(idx[frameAxis]);
        ITK_TEST_EXPECT_EQUAL(frame->GetPixel(itk::MakeIndex(idx[1], idx[2])), it.Get());

        ++it;
      }
      it.NextLine();
    }
    it.NextSlice();
  }

  // Write out one frame from output VideoStream for baseline comparison
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(videoOutput->GetFrame(0), argv[2]));

  return EXIT_SUCCESS;
}
