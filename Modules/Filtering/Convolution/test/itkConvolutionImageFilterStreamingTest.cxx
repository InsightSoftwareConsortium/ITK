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

#include "itkConvolutionImageFilter.h"
#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkGaussianImageSource.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"

using KernelImageType = itk::Image<float, 2>;

typename KernelImageType::Pointer
GenerateKernelForStreamingTest()
{
  using SourceType = itk::GaussianImageSource<KernelImageType>;
  using KernelSizeType = typename SourceType::SizeType;
  auto           source = SourceType::New();
  KernelSizeType kernelSize{ 3, 5 };
  source->SetSize(kernelSize);
  source->SetMean(2);
  source->SetSigma(3.0);
  source->SetScale(1.0);
  source->SetNormalized(true);
  source->Update();
  return source->GetOutput();
}

template <typename ConvolutionFilterType>
int
doConvolutionImageFilterStreamingTest(int argc, char * argv[])
{
  constexpr int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using RegionType = typename ImageType::RegionType;
  using SizeType = typename RegionType::SizeType;
  using IndexType = typename RegionType::IndexType;

  // Request a subregion of the largest possible output
  IndexType requestedIndex;
  requestedIndex[0] = std::atoi(argv[4]);
  requestedIndex[1] = std::atoi(argv[5]);
  SizeType requestedSize;
  requestedSize[0] = std::atoi(argv[6]);
  requestedSize[1] = std::atoi(argv[7]);
  RegionType requestedRegion(requestedIndex, requestedSize);

  auto regionMode = (argc > 9 && std::string("valid").compare(argv[8]) == 0
                       ? itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID
                       : itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME);

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[2]);

  // Verify the requested region is a subregion of the largest possible region
  reader1->UpdateOutputInformation();
  ITK_TEST_EXPECT_TRUE(reader1->GetOutput()->GetLargestPossibleRegion().IsInside(requestedRegion));

  using PipelineMonitorType = itk::PipelineMonitorImageFilter<ImageType>;
  auto inputMonitor = PipelineMonitorType::New();
  inputMonitor->SetInput(reader1->GetOutput());

  KernelImageType::Pointer kernelImage = GenerateKernelForStreamingTest();

  auto convoluter = ConvolutionFilterType::New();
  convoluter->SetInput(inputMonitor->GetOutput());
  convoluter->SetKernelImage(kernelImage);
  convoluter->SetOutputRegionMode(regionMode);
  convoluter->SetReleaseDataFlag(true);
  itk::SimpleFilterWatcher watcher(convoluter, "filter");

  using PipelineMonitorType = itk::PipelineMonitorImageFilter<ImageType>;
  auto outputMonitor = PipelineMonitorType::New();
  outputMonitor->SetInput(convoluter->GetOutput());

  using StreamingFilterType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamingFilterType::New();
  streamer->SetInput(outputMonitor->GetOutput());
  const unsigned int numStreamDivisions = 10;
  streamer->SetNumberOfStreamDivisions(numStreamDivisions);
  streamer->GetOutput()->SetRequestedRegion(requestedRegion);
  ITK_TRY_EXPECT_NO_EXCEPTION(streamer->Update());

  // Verify the pipeline executed as expected
  // Expect requested region propagates twice due to ConvolutionImageFilter having
  // two inputs (Primary and KernelImage)
  const unsigned int requestsPerStream = 2;
  // Verify ConvolutionImageFilter upstream requests
  ITK_TEST_EXPECT_EQUAL(inputMonitor->GetOutputRequestedRegions().size(), requestsPerStream * numStreamDivisions);
  ITK_TEST_EXPECT_EQUAL(inputMonitor->GetNumberOfUpdates(), numStreamDivisions);

  // Verify ConvolutionImageFilter downstream outputs
  ITK_TEST_EXPECT_EQUAL(outputMonitor->GetNumberOfUpdates(), numStreamDivisions);
  ITK_TEST_EXPECT_EQUAL(outputMonitor->GetOutputRequestedRegions().size(), numStreamDivisions);
  ITK_TEST_EXPECT_TRUE(outputMonitor->VerifyInputFilterBufferedRequestedRegions());
  ITK_TEST_EXPECT_TRUE(outputMonitor->VerifyAllInputCanStream(numStreamDivisions));
  ITK_TEST_EXPECT_TRUE(outputMonitor->VerifyInputFilterMatchedUpdateOutputInformation());

  //// Write out buffered region resulting from convolution
  using RegionOfInterestFilterType = itk::RegionOfInterestImageFilter<ImageType, ImageType>;
  auto cropFilter = RegionOfInterestFilterType::New();
  cropFilter->SetInput(streamer->GetOutput());
  cropFilter->SetRegionOfInterest(streamer->GetOutput()->GetBufferedRegion());
  cropFilter->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(cropFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  std::cout << "Wrote output image to " << argv[3] << std::endl;

  return EXIT_SUCCESS;
}

int
itkConvolutionImageFilterStreamingTest(int argc, char * argv[])
{

  if (argc < 8)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << "convolutionType "
              << "inputImage "
              << "outputImage "
              << "indexX indexY sizeX sizeY "
              << "[regionMode]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  if (std::string("frequency").compare(argv[1]) == 0)
  {
    using FrequencyConvolutionType = itk::FFTConvolutionImageFilter<ImageType>;

    // Do a quick filter sanity check before the test
    auto convoluter = FrequencyConvolutionType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(convoluter, FFTConvolutionImageFilter, ConvolutionImageFilterBase);

    return doConvolutionImageFilterStreamingTest<FrequencyConvolutionType>(argc, argv);
  }
  else // spatial
  {
    using SpatialConvolutionType = itk::ConvolutionImageFilter<ImageType>;

    // Do a quick filter sanity check before the test
    auto convoluter = SpatialConvolutionType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(convoluter, ConvolutionImageFilter, ConvolutionImageFilterBase);

    return doConvolutionImageFilterStreamingTest<SpatialConvolutionType>(argc, argv);
  }
}
