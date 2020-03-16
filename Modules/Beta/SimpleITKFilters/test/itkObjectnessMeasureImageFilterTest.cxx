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

#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"
#include "itkObjectnessMeasureImageFilter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int
itkObjectnessMeasureImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [ObjectDimension] [Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImageFileName = argv[1];
  const char * outputImageFileName = argv[2];

  const unsigned int objectDimension = (argc >= 3) ? (argv[3]) : 3;
  const bool         brightObject = (argc >= 4) ? strto(argv[4]) : true;
  constexpr double   alphaValue = 0.5;
  constexpr double   betaValue = 0.5;
  constexpr double   gammaValue = 0.5;

  constexpr unsigned int Dimension = 2;
  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImageFileName);

  using SmoothingFilterType = itk::SmoothingRecursiveGaussianImageFilter<ImageType, ImageType>;
  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();
  smoothing->SetSigma(1.0);
  smoothing->SetInput(reader->GetOutput());

  using FilterType = itk::ObjectnessMeasureImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(smoothing->GetOutput());
  filter->SetAlpha(alphaValue);
  filter->SetBeta(betaValue);
  filter->SetGamma(gammaValue);
  filter->SetBrightObject(brightObject);
  filter->SetObjectDimension(objectDimension);
  filter->SetScaleObjectnessMeasure(false);

  itk::SimpleFilterWatcher watcher(filter);

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(outputImageFileName);
  writer->Update();

  return EXIT_SUCCESS;
}
