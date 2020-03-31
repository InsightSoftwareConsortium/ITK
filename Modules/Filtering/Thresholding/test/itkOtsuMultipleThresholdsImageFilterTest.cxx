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

#include "itkImageFileReader.h"
#include "itkOtsuMultipleThresholdsImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkTestingMacros.h"


int
itkOtsuMultipleThresholdsImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " numberOfHistogramBins"
              << " numberOfThresholds"
              << " labelOffset"
              << " [valleyEmphasis]"
              << " [returnBinMidpoint]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = short;
  using InternalPixelType = unsigned short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using FilterType = itk::OtsuMultipleThresholdsImageFilter<InputImageType, InternalImageType>;
  using RescaleType = itk::RescaleIntensityImageFilter<InternalImageType, OutputImageType>;

  // Set up the reader
  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using FilterType = itk::OtsuMultipleThresholdsImageFilter<InputImageType, InternalImageType>;
  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, OtsuMultipleThresholdsImageFilter, ImageToImageFilter);

  // Set up the filter parameters

#if defined(ITKV4_COMPATIBILITY)
  ITK_TEST_EXPECT_TRUE(filter->GetReturnBinMidpoint());
#else
  ITK_TEST_EXPECT_TRUE(!filter->GetReturnBinMidpoint());
#endif

  filter->ReturnBinMidpointOff();

  auto numberOfHistogramBins = static_cast<itk::SizeValueType>(std::stoi(argv[3]));
  filter->SetNumberOfHistogramBins(numberOfHistogramBins);
  ITK_TEST_SET_GET_VALUE(numberOfHistogramBins, filter->GetNumberOfHistogramBins());

  auto numberOfThresholds = static_cast<itk::SizeValueType>(std::stoi(argv[4]));
  filter->SetNumberOfThresholds(numberOfThresholds);
  ITK_TEST_SET_GET_VALUE(numberOfThresholds, filter->GetNumberOfThresholds());

  auto labelOffset = static_cast<FilterType::OutputPixelType>(std::stoi(argv[5]));
  filter->SetLabelOffset(labelOffset);
  ITK_TEST_SET_GET_VALUE(labelOffset, filter->GetLabelOffset());

  if (argc > 6)
  {
    bool valleyEmphasis = static_cast<bool>(std::stoi(argv[6]));
    ITK_TEST_SET_GET_BOOLEAN(filter, ValleyEmphasis, valleyEmphasis);
  }

  if (argc > 7)
  {
    bool returnBinMidpoint = static_cast<bool>(std::stoi(argv[7]));
    ITK_TEST_SET_GET_BOOLEAN(filter, ReturnBinMidpoint, returnBinMidpoint);
  }

  FilterType::ThresholdVectorType thresholds = filter->GetThresholds();
  std::cout << "filter->GetThresholds(): ";
  for (unsigned int i = 0; i < thresholds.size(); i++)
  {
    std::cout << itk::NumericTraits<FilterType::InputPixelType>::PrintType(thresholds[i]) << " ";
  }
  std::cout << std::endl;


  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Rescale the image so that it can be seen.  The output of the
  // filter contains labels that are numbered sequentially, so the
  // image looks nearly uniform unless there are a large number of labels.
  //
  using RescaleType = itk::RescaleIntensityImageFilter<InternalImageType, OutputImageType>;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput(filter->GetOutput());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Write out the test image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
