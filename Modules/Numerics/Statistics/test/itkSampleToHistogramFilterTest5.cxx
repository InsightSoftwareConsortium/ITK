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

#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"
#include "itkImageToListSampleFilter.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int
itkSampleToHistogramFilterTest5(int argc, char * argv[])
{

  constexpr unsigned int imageDimension = 2;

  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "  inputImageFilename " << std::endl;
    return EXIT_FAILURE;
  }

  //
  // Note:
  //
  // The purpose of this test is to verify that the
  // SampleToHistogramFilter can be used for generating the
  // histogram of an image.
  //
  using VMeasurementType = unsigned char; // type for the samples
  using HMeasurementType = float;         // type for the histogram


  using PixelType = itk::RGBPixel<VMeasurementType>;

  constexpr unsigned int numberOfComponents = 3;

  using ImageType = itk::Image<PixelType, imageDimension>;

  using ImageToListSampleFilterType = itk::Statistics::ImageToListSampleFilter<ImageType>;

  using SampleType = ImageToListSampleFilterType::ListSampleType;

  using HistogramType = itk::Statistics::Histogram<HMeasurementType, itk::Statistics::DenseFrequencyContainer2>;

  using FilterType = itk::Statistics::SampleToHistogramFilter<SampleType, HistogramType>;

  using MeasurementVectorType = HistogramType::MeasurementVectorType;

  using HistogramSizeType = FilterType::HistogramSizeType;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();

  auto imageToSampleFilter = ImageToListSampleFilterType::New();

  auto filter = FilterType::New();

  reader->SetFileName(argv[1]);

  imageToSampleFilter->SetInput(reader->GetOutput());

  filter->SetInput(imageToSampleFilter->GetOutput());

  // Test exception when calling Update() without having
  // defined the size of the histogram in the filter.
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  const HistogramType * histogram = filter->GetOutput();

  if (histogram->Size() != 0)
  {
    std::cerr << "Histogram Size should have been zero" << std::endl;
    return EXIT_FAILURE;
  }

  HistogramSizeType histogramSize(numberOfComponents);

  histogramSize[0] = 256;
  histogramSize[1] = 256;
  histogramSize[2] = 256;

  filter->SetHistogramSize(histogramSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  const unsigned int expectedHistogramSize = histogramSize[0] * histogramSize[1] * histogramSize[2];

  if (histogram->Size() != expectedHistogramSize)
  {
    std::cerr << "Histogram Size should have been " << expectedHistogramSize << std::endl;
    std::cerr << " but it actually is " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
  }


  HistogramType::ConstIterator histogramItr = histogram->Begin();
  HistogramType::ConstIterator histogramEnd = histogram->End();

  using PrintType = itk::NumericTraits<VMeasurementType>::PrintType;

  unsigned int count = 0;
  while (histogramItr != histogramEnd)
  {
    if (histogramItr.GetFrequency() != 0)
    {
      count++;
      if (count % 1000)
      {
        MeasurementVectorType measurementVector = histogramItr.GetMeasurementVector();
        std::cout << static_cast<PrintType>(measurementVector[0]) << "  ";
        std::cout << static_cast<PrintType>(measurementVector[1]) << "  ";
        std::cout << static_cast<PrintType>(measurementVector[2]) << "  ";
        std::cout << "frequency: " << histogramItr.GetFrequency() << std::endl;
      }
    }
    ++histogramItr;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
