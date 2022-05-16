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
#include "itkImageFileReader.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkImageToHistogramFilterTest2(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputRGBImageFileName outputHistogramFile.txt [autoMinumumMaximum]" << std::endl;
    return EXIT_FAILURE;
  }


  bool autoMinimumMaximum = false;
  if (argc >= 4)
  {
    autoMinimumMaximum = atoi(argv[3]);
  }

  using PixelComponentType = unsigned char;

  using RGBPixelType = itk::RGBPixel<PixelComponentType>;

  constexpr unsigned int Dimension = 2;

  using RGBImageType = itk::Image<RGBPixelType, Dimension>;

  constexpr unsigned int MeasurementVectorSize = 3; // RGB

  using ReaderType = itk::ImageFileReader<RGBImageType>;

  auto reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  using HistogramFilterType = itk::Statistics::ImageToHistogramFilter<RGBImageType>;

  auto                     histogramFilter = HistogramFilterType::New();
  itk::SimpleFilterWatcher watcher(histogramFilter, "filter");

  using HistogramMeasurementVectorType = HistogramFilterType::HistogramMeasurementVectorType;

  if (autoMinimumMaximum)
  {
    histogramFilter->AutoMinimumMaximumOn();
  }
  else
  {
    // Setting bin mins and max
    HistogramMeasurementVectorType histogramBinMinimum(MeasurementVectorSize);
    histogramBinMinimum[0] = -0.5;
    histogramBinMinimum[1] = -0.5;
    histogramBinMinimum[2] = -0.5;

    HistogramMeasurementVectorType histogramBinMaximum(MeasurementVectorSize);
    histogramBinMaximum[0] = 255.5;
    histogramBinMaximum[1] = 255.5;
    histogramBinMaximum[2] = 255.5;

    histogramFilter->SetHistogramBinMinimum(histogramBinMinimum);
    histogramFilter->SetHistogramBinMaximum(histogramBinMaximum);
  }

  using SizeType = HistogramFilterType::HistogramSizeType;

  SizeType size(MeasurementVectorSize);

  size[0] = 255; // number of bins for the Red   channel
  size[1] = 1;   // number of bins for the Green channel
  size[2] = 1;   // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize(size);

  histogramFilter->SetMarginalScale(10.0);

  histogramFilter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(histogramFilter->Update());


  using HistogramType = HistogramFilterType::HistogramType;
  const HistogramType * histogram = histogramFilter->GetOutput();

  std::ofstream outputFile;
  outputFile.open(argv[2]);

  const unsigned int histogramSize = histogram->Size();
  outputFile << "Histogram size " << histogramSize << std::endl;


  unsigned int channel = 0; // red channel

  outputFile << "Histogram of the red component" << std::endl;

  for (unsigned int bin = 0; bin < histogramSize; ++bin)
  {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency(bin, channel) << std::endl;
  }


  size[0] = 1;   // number of bins for the Red   channel
  size[1] = 255; // number of bins for the Green channel
  size[2] = 1;   // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize(size);

  ITK_TRY_EXPECT_NO_EXCEPTION(histogramFilter->Update());


  channel = 1; // green channel

  outputFile << "Histogram of the green component" << std::endl;

  for (unsigned int bin = 0; bin < histogramSize; ++bin)
  {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency(bin, channel) << std::endl;
  }


  size[0] = 1;   // number of bins for the Red   channel
  size[1] = 1;   // number of bins for the Green channel
  size[2] = 255; // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize(size);

  ITK_TRY_EXPECT_NO_EXCEPTION(histogramFilter->Update());


  channel = 2; // blue channel

  outputFile << "Histogram of the blue component" << std::endl;

  for (unsigned int bin = 0; bin < histogramSize; ++bin)
  {
    outputFile << "bin = " << bin << " frequency = ";
    outputFile << histogram->GetFrequency(bin, channel) << std::endl;
  }

  outputFile.close();

  return EXIT_SUCCESS;
}
