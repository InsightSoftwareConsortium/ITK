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

#include "itkScalarImageToHistogramGenerator.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkHistogramToEntropyImageFilter.h"
#include "itkTestingMacros.h"

int
itkHistogramToEntropyImageFilterTest1(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputScalarImageFileName outputImage" << std::endl;
    return EXIT_FAILURE;
  }


  constexpr unsigned int Dimension = 2;
  using PixelComponentType = unsigned char;
  using ScalarImageType = itk::Image<PixelComponentType, Dimension>;
  using ReaderType = itk::ImageFileReader<ScalarImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  itk::MinimumMaximumImageFilter<ScalarImageType>::Pointer minmaxFilter =
    itk::MinimumMaximumImageFilter<ScalarImageType>::New();
  minmaxFilter->SetInput(reader->GetOutput());
  minmaxFilter->Update();
  const ScalarImageType::PixelType imageMin = minmaxFilter->GetMinimum();
  const ScalarImageType::PixelType imageMax = minmaxFilter->GetMaximum();


  using HistogramGeneratorType = itk::Statistics::ScalarImageToHistogramGenerator<ScalarImageType>;
  auto histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput(reader->GetOutput());

  const int NumberOfBins = static_cast<unsigned int>(imageMax - imageMin + 1);
  histogramGenerator->SetNumberOfBins(NumberOfBins);
  histogramGenerator->SetMarginalScale(1.0);

  histogramGenerator->SetHistogramMin(imageMin);
  histogramGenerator->SetHistogramMax(imageMax);

  histogramGenerator->Compute();


  using HistogramType = HistogramGeneratorType::HistogramType;
  const HistogramType * histogram = histogramGenerator->GetOutput();

  using HistogramToImageFilterType = itk::HistogramToEntropyImageFilter<HistogramType>;

  auto histogramToImageFilter = HistogramToImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(histogramToImageFilter, HistogramToEntropyImageFilter, HistogramToImageFilter);


  histogramToImageFilter->SetInput(histogram);

  using OutputImageType = HistogramToImageFilterType::OutputImageType;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);

  writer->SetInput(histogramToImageFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
