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

#include "itkImageToHistogramFilter.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJoinImageFilter.h"
#include "itkHistogramToProbabilityImageFilter.h"

int
itkHistogramToProbabilityImageFilterTest2(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputScalarImageFileName outputImage" << std::endl;
    return EXIT_FAILURE;
  }


  constexpr unsigned int Dimension = 2;
  using PixelComponentType = unsigned char;

  using ScalarImageType = itk::Image<PixelComponentType, Dimension>;
  using ReaderType = itk::ImageFileReader<ScalarImageType>;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName(argv[1]);
  reader2->SetFileName(argv[2]);

  try
  {
    reader1->Update();
    reader2->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using JoinFilterType = itk::JoinImageFilter<ScalarImageType, ScalarImageType>;

  JoinFilterType::Pointer joinFilter = JoinFilterType::New();

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
  minmaxFilter->Update();

  imageMin[0] = minmaxFilter->GetMinimum();
  imageMax[0] = minmaxFilter->GetMaximum();

  minmaxFilter->SetInput(reader2->GetOutput());
  minmaxFilter->Update();

  imageMin[1] = minmaxFilter->GetMinimum();
  imageMax[1] = minmaxFilter->GetMaximum();


  HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();

  histogramFilter->SetInput(joinFilter->GetOutput());

  HistogramFilterType::HistogramSizeType numberOfBins(NumberOfComponents);

  numberOfBins[0] = static_cast<unsigned int>(imageMax[0] - imageMin[0] + 1);
  numberOfBins[1] = static_cast<unsigned int>(imageMax[1] - imageMin[1] + 1);

  histogramFilter->SetHistogramSize(numberOfBins);

  histogramFilter->SetMarginalScale(1.0);

  histogramFilter->SetHistogramBinMinimum(imageMin);
  histogramFilter->SetHistogramBinMaximum(imageMax);

  histogramFilter->Update();


  using HistogramType = HistogramFilterType::HistogramType;
  const HistogramType * histogram = histogramFilter->GetOutput();

  using HistogramToImageFilterType = itk::HistogramToProbabilityImageFilter<HistogramType>;
  HistogramToImageFilterType::Pointer histogramToImageFilter = HistogramToImageFilterType::New();

  histogramToImageFilter->SetInput(histogram);

  using OutputImageType = HistogramToImageFilterType::OutputImageType;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[3]);

  writer->SetInput(histogramToImageFilter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
