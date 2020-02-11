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

#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkLaplacianRecursiveGaussianImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage " << std::endl;
    return -1;
  }

  const char * inputFilename = argv[1];
  const char * outputFilename = argv[2];

  using CharPixelType = unsigned char; // IO
  using RealPixelType = double;        // Operations

  constexpr unsigned int Dimension = 2;

  using CharImageType = itk::Image<CharPixelType, Dimension>;
  using RealImageType = itk::Image<RealPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<CharImageType>;
  using WriterType = itk::ImageFileWriter<CharImageType>;

  using CastToRealFilterType = itk::CastImageFilter<CharImageType, RealImageType>;
  using CastToCharFilterType = itk::CastImageFilter<RealImageType, CharImageType>;

  using RescaleFilter = itk::RescaleIntensityImageFilter<RealImageType, RealImageType>;

  using LaplacianFilter = itk::LaplacianRecursiveGaussianImageFilter<RealImageType, RealImageType>;

  { // Instantiate a 6D image for testing purposes
    using HighDImageType = itk::Image<RealPixelType, 6>;
    using LaplacianFilterHighDType = itk::LaplacianRecursiveGaussianImageFilter<HighDImageType, HighDImageType>;
    LaplacianFilterHighDType::Pointer nDTest = LaplacianFilterHighDType::New();
  }

  using ZeroCrossingFilter = itk::ZeroCrossingImageFilter<RealImageType, RealImageType>;

  // Setting the IO
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  CastToCharFilterType::Pointer toChar = CastToCharFilterType::New();
  RescaleFilter::Pointer        rescale = RescaleFilter::New();

  // Setting the ITK pipeline filter

  LaplacianFilter::Pointer    lapFilter = LaplacianFilter::New();
  itk::SimpleFilterWatcher    watcher(lapFilter);
  ZeroCrossingFilter::Pointer zeroFilter = ZeroCrossingFilter::New();

  reader->SetFileName(inputFilename);
  writer->SetFileName(outputFilename);

  // The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  toReal->SetInput(reader->GetOutput());
  toChar->SetInput(rescale->GetOutput());
  writer->SetInput(toChar->GetOutput());

  // Edge Detection by Laplacian Image Filter:

  lapFilter->SetInput(toReal->GetOutput());
  lapFilter->SetSigma(2.0);
  zeroFilter->SetInput(lapFilter->GetOutput());
  rescale->SetInput(zeroFilter->GetOutput());

  // Test itkGetMacro
  bool bNormalizeAcrossScale = lapFilter->GetNormalizeAcrossScale();
  std::cout << "lapFilter->GetNormalizeAcrossScale(): " << bNormalizeAcrossScale << std::endl;

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
  }

  return EXIT_SUCCESS;
}
