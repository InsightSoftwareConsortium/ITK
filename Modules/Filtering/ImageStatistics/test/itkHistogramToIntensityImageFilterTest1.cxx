/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkScalarImageToHistogramGenerator.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkHistogramToIntensityImageFilter.h"
int
itkHistogramToIntensityImageFilterTest1(int argc, char * argv[])
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

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  itk::MinimumMaximumImageFilter<ScalarImageType>::Pointer minmaxFilter =
    itk::MinimumMaximumImageFilter<ScalarImageType>::New();
  minmaxFilter->SetInput(reader->GetOutput());
  minmaxFilter->Update();
  const ScalarImageType::PixelType imageMin = minmaxFilter->GetMinimum();
  const ScalarImageType::PixelType imageMax = minmaxFilter->GetMaximum();


  using HistogramGeneratorType = itk::Statistics::ScalarImageToHistogramGenerator<ScalarImageType>;
  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput(reader->GetOutput());

  const int NumberOfBins = static_cast<unsigned int>(imageMax - imageMin + 1);
  histogramGenerator->SetNumberOfBins(NumberOfBins);
  histogramGenerator->SetMarginalScale(1.0);

  histogramGenerator->SetHistogramMin(imageMin);
  histogramGenerator->SetHistogramMax(imageMax);

  histogramGenerator->Compute();


  using HistogramType = HistogramGeneratorType::HistogramType;
  const HistogramType * histogram = histogramGenerator->GetOutput();

  using HistogramToImageFilterType = itk::HistogramToIntensityImageFilter<HistogramType>;
  HistogramToImageFilterType::Pointer histogramToImageFilter = HistogramToImageFilterType::New();

  histogramToImageFilter->SetInput(histogram);

  using OutputImageType = HistogramToImageFilterType::OutputImageType;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[2]);

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
