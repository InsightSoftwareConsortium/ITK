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

#include <fstream>
#include "itkVectorThresholdSegmentationLevelSetImageFilter.h"
#include "itkImageFileReader.h"
#include "itkTextOutput.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkMath.h"

int
itkVectorThresholdSegmentationLevelSetImageFilterTest(int ac, char * av[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (ac < 6)
  {
    std::cerr << "Usage: " << av[0] << " InputInitialImage InputColorImage BaselineImage threshold curvatureScaling\n";
    return -1;
  }

  constexpr unsigned int Dimension = 2;

  using PixelComponentType = unsigned char;
  using RGBPixelType = itk::RGBPixel<PixelComponentType>;
  using InputPixelType = unsigned char;
  using OutputPixelType = float;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using RGBImageType = itk::Image<RGBPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;

  using InputReaderType = itk::ImageFileReader<InputImageType>;
  using RGBReaderType = itk::ImageFileReader<RGBImageType>;

  RGBReaderType::Pointer   rgbReader = RGBReaderType::New();
  InputReaderType::Pointer inputReader = InputReaderType::New();

  inputReader->SetFileName(av[1]);
  rgbReader->SetFileName(av[2]);

  // Create a filter
  using FilterType = itk::VectorThresholdSegmentationLevelSetImageFilter<InputImageType, RGBImageType, OutputPixelType>;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(inputReader->GetOutput());

  filter->SetFeatureImage(rgbReader->GetOutput());

  // Mean values hand coded for the VisibleWomanSlice.png color file
  using MeanVectorType = FilterType::MeanVectorType;
  MeanVectorType mean = MeanVectorType(3);

  mean[0] = 44.7504;
  mean[1] = 37.5443;
  mean[2] = 29.5179;

  filter->SetMean(mean);

  // Covariance values hand coded for the VisibleWomanSlice.png color file
  using CovarianceMatrixType = FilterType::CovarianceMatrixType;
  CovarianceMatrixType covariance = CovarianceMatrixType(3, 3);

  covariance[0][0] = 79.2225;
  covariance[1][1] = 81.0314;
  covariance[2][2] = 51.1744;
  covariance[0][1] = 72.4737;
  covariance[0][2] = 57.7892;
  covariance[1][2] = 61.9859;
  covariance[1][0] = covariance[0][1];
  covariance[2][0] = covariance[0][2];
  covariance[2][1] = covariance[1][2];

  filter->SetCovariance(covariance);

  const double threshold = std::stod(av[4]);

  filter->SetThreshold(threshold);

  const double curvatureScaling = std::stod(av[5]);

  filter->SetCurvatureScaling(curvatureScaling);

  try
  {
    rgbReader->Update();
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }

  // Test the GetMacros
  if (itk::Math::NotExactlyEquals(filter->GetThreshold(), threshold))
  {
    std::cerr << "Error GetThreshold returns a value";
    std::cerr << " different from the one in SetThreshold" << std::endl;
    std::cerr << "threshold      = " << threshold << std::endl;
    std::cerr << "GetThreshold() = " << filter->GetThreshold() << std::endl;
    return EXIT_FAILURE;
  }


  using RescalerType = itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;
  RescalerType::Pointer rescaler = RescalerType::New();

  rescaler->SetInput(filter->GetOutput());

  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Generate test image
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(rescaler->GetOutput());
  writer->SetFileName(av[3]);
  writer->Update();

  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;
}
