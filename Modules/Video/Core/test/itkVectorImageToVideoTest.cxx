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

#include "itkVectorImage.h"
#include "itkVideoStream.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

int
itkVectorImageToVideoTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFile outputFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  const unsigned int Dimension = 2;

  using ImageType = itk::VectorImage<PixelType, Dimension>;
  using FilterOutputType = itk::VideoStream<itk::VectorImage<PixelType, Dimension - 1>>;

  // Get 3D image to represent a temporal dataset of 2D frames
  const auto inputImage = itk::ReadImage<ImageType>(argv[1]);

  using VideoFilterType = itk::ImageToVideoFilter<ImageType, FilterOutputType>;
  auto videoFilter = VideoFilterType::New();
  videoFilter->SetInput(inputImage);
  // Arbitrarily set last axis as temporal dimension to split frames
  itk::IndexValueType frameAxis = Dimension - 1;
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
  auto observedDuration =
    static_cast<float>(videoOutput->GetLargestPossibleTemporalRegion().GetRealDuration().GetTimeInSeconds());
  auto expectedDuration =
    static_cast<float>(inputImage->GetSpacing()[frameAxis] * inputImage->GetLargestPossibleRegion().GetSize(frameAxis));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(observedDuration, expectedDuration));

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
  outputDirection.SetIdentity();
  for (auto frameIdx = videoOutput->GetLargestPossibleTemporalRegion().GetFrameStart();
       frameIdx < videoOutput->GetLargestPossibleTemporalRegion().GetFrameDuration();
       ++frameIdx)
  {
    ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(frameIdx)->GetDirection(), outputDirection);
  }

  // Verify pixel component matches input
  ITK_TEST_EXPECT_EQUAL(videoOutput->GetFrame(0)->GetNumberOfComponentsPerPixel(),
                        inputImage->GetNumberOfComponentsPerPixel());

  // Iterate over 3D input + video output to verify pixel data matches across each slice/frame
  using ImageIteratorType = typename itk::ImageLinearConstIteratorWithIndex<ImageType>;
  ImageIteratorType it(inputImage, inputImage->GetLargestPossibleRegion());
  it.SetDirection(1); // Slice direction
  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      auto idx = it.GetIndex();
      auto frame = videoOutput->GetFrame(idx[frameAxis]);
      ITK_TEST_EXPECT_EQUAL(frame->GetPixel(itk::MakeIndex(idx[0])), it.Get());

      ++it;
    }
    it.NextLine();
  }

  // Write out one frame from output VideoStream for baseline comparison
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(videoOutput->GetFrame(0), argv[2]));

  return EXIT_SUCCESS;
}
