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

#include "itkImageToHistogramFilter.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJoinImageFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkTestingMacros.h"

int
itkHistogramToLogProbabilityImageFilterTest2(int argc, char * argv[])
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

  auto reader1 = ReaderType::New();
  auto reader2 = ReaderType::New();

  reader1->SetFileName(argv[1]);
  reader2->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());
  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());


  using JoinFilterType = itk::JoinImageFilter<ScalarImageType, ScalarImageType>;

  auto joinFilter = JoinFilterType::New();

  using ArrayImageType = JoinFilterType::OutputImageType;

  joinFilter->SetInput1(reader1->GetOutput());
  joinFilter->SetInput2(reader2->GetOutput());

  using HistogramFilterType = itk::Statistics::ImageToHistogramFilter<ArrayImageType>;

  using HistogramMeasurementVectorType = HistogramFilterType::HistogramMeasurementVectorType;

  constexpr unsigned int NumberOfComponents = 2;

  itk::MinimumMaximumImageFilter<ScalarImageType>::Pointer minmaxFilter =
    itk::MinimumMaximumImageFilter<ScalarImageType>::New();

  HistogramMeasurementVectorType imageMin(NumberOfComponents);
  HistogramMeasurementVectorType imageMax(NumberOfComponents);

  minmaxFilter->SetInput(reader1->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(minmaxFilter->Update());

  imageMin[0] = minmaxFilter->GetMinimum();
  imageMax[0] = minmaxFilter->GetMaximum();

  minmaxFilter->SetInput(reader2->GetOutput());
  minmaxFilter->Update();

  imageMin[1] = minmaxFilter->GetMinimum();
  imageMax[1] = minmaxFilter->GetMaximum();


  auto histogramFilter = HistogramFilterType::New();

  histogramFilter->SetInput(joinFilter->GetOutput());

  HistogramFilterType::HistogramSizeType numberOfBins(NumberOfComponents);

  numberOfBins[0] = static_cast<unsigned int>(imageMax[0] - imageMin[0] + 1);
  numberOfBins[1] = static_cast<unsigned int>(imageMax[1] - imageMin[1] + 1);

  histogramFilter->SetHistogramSize(numberOfBins);

  histogramFilter->SetMarginalScale(1.0);

  histogramFilter->SetHistogramBinMinimum(imageMin);
  histogramFilter->SetHistogramBinMaximum(imageMax);

  ITK_TRY_EXPECT_NO_EXCEPTION(histogramFilter->Update());


  using HistogramType = HistogramFilterType::HistogramType;
  const HistogramType * histogram = histogramFilter->GetOutput();

  using HistogramToImageFilterType = itk::HistogramToLogProbabilityImageFilter<HistogramType>;
  auto histogramToImageFilter = HistogramToImageFilterType::New();

  histogramToImageFilter->SetInput(histogram);

  using OutputImageType = HistogramToImageFilterType::OutputImageType;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[3]);

  writer->SetInput(histogramToImageFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
