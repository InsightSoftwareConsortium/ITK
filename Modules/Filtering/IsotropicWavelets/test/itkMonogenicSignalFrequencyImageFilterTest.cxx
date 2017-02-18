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

#include "itkMonogenicSignalFrequencyImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkTestingMacros.h"

#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif


int
itkMonogenicSignalFrequencyImageFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inputImage = argv[1];
  const std::string outputImage = argv[2];

  const unsigned int                       Dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(inputImage);

  TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  FFTFilterType::Pointer                        fftFilter = FFTFilterType::New();

  fftFilter->SetInput(reader->GetOutput());

  fftFilter->Update();

  typedef FFTFilterType::OutputImageType ComplexImageType;

  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType> MonogenicSignalFilterType;
  MonogenicSignalFilterType::Pointer                                 monoFilter = MonogenicSignalFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(monoFilter, MonogenicSignalFrequencyImageFilter, ImageToImageFilter);

  monoFilter->SetInput(fftFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(monoFilter->Update());

  unsigned int expectedNumberOfComponentsPerPixel = monoFilter->GetOutput()->GetNumberOfComponentsPerPixel();
  unsigned int computedNumberOfComponentsPerPixel = Dimension + 1;
  if (expectedNumberOfComponentsPerPixel != computedNumberOfComponentsPerPixel)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Wrong number of components" << std::endl;
    std::cerr << "Expected: " << expectedNumberOfComponentsPerPixel
              << ", but got: " << computedNumberOfComponentsPerPixel << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
