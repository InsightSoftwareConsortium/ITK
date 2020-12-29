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

#include "itkMonogenicSignalFrequencyImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingMacros.h"

#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkNumberToString.h"
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

  bool testPassed = true;

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Perform FFT on input image.
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();
  using ComplexImageType = FFTFilterType::OutputImageType;

  using MonogenicSignalFilterType = itk::MonogenicSignalFrequencyImageFilter<ComplexImageType>;
  auto monoFilter = MonogenicSignalFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(monoFilter, MonogenicSignalFrequencyImageFilter, ImageToImageFilter);

  monoFilter->SetInput(fftFilter->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(monoFilter->Update());

  unsigned int expectedNumberOfComponentsPerPixel = monoFilter->GetOutput()->GetNumberOfComponentsPerPixel();
  unsigned int computedNumberOfComponentsPerPixel = Dimension + 1;
  if (expectedNumberOfComponentsPerPixel != computedNumberOfComponentsPerPixel)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Wrong number of components" << std::endl;
    std::cerr << "Expected: " << expectedNumberOfComponentsPerPixel
              << ", but got: " << computedNumberOfComponentsPerPixel << std::endl;
    testPassed = false;
  }

  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();

  using MonoFilterOutputImageType = MonogenicSignalFilterType::OutputImageType;
  using VectorCastFilterType = itk::VectorIndexSelectionCastImageFilter<MonoFilterOutputImageType, ComplexImageType>;
  auto vectorCastFilter = VectorCastFilterType::New();
  vectorCastFilter->SetInput(monoFilter->GetOutput());
  for (unsigned int c = 0; c < computedNumberOfComponentsPerPixel; ++c)
  {
    vectorCastFilter->SetIndex(c);
    vectorCastFilter->Update();
    inverseFFT->SetInput(vectorCastFilter->GetOutput());
    inverseFFT->Update();
#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<unsigned int> n2s;
    itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "MonoFilterOutput (inverseFFT). Component: " + n2s(c));
#endif
  }

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
