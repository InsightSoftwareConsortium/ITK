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
#include <string>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

#include "itkVectorInverseFFTImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingComparisonImageFilter.h"

using namespace std;
using namespace itk;

int
itkVectorInverseFFTImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage " << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];

  constexpr unsigned int dimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  using FFTForwardFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  using ComplexImageType = FFTForwardFilterType::OutputImageType;

  // Perform inverse FFT on forwardFFT image.
  using FFTInverseFilterType = itk::InverseFFTImageFilter<ComplexImageType>;
  auto fftInverseFilter = FFTInverseFilterType::New();
  fftInverseFilter->SetInput(fftForwardFilter->GetOutput());
  fftInverseFilter->Update();

  // Create VectorImages
  //
  unsigned int numComponents = 2;
  // Create vector from forwardFFT.
  using ComposeComplexFilterType = itk::ComposeImageFilter<ComplexImageType>;
  auto composeComplexFilter = ComposeComplexFilterType::New();
  for (unsigned int c = 0; c < numComponents; c++)
  {
    composeComplexFilter->SetInput(c, fftForwardFilter->GetOutput());
  }
  composeComplexFilter->Update();

  // Do the inverse of the composeComplexFilter using the class to test.
  using VectorInverseFFTType = itk::VectorInverseFFTImageFilter<ComposeComplexFilterType::OutputImageType>;
  auto vecInverseFFT = VectorInverseFFTType::New();
  vecInverseFFT->SetInput(composeComplexFilter->GetOutput());
  vecInverseFFT->Update();

  using VectorCastFilterType = itk::VectorIndexSelectionCastImageFilter<VectorInverseFFTType::OutputImageType,
                                                                        FFTInverseFilterType::OutputImageType>;
  auto vectorCastFilter = VectorCastFilterType::New();
  vectorCastFilter->SetInput(vecInverseFFT->GetOutput());

  using DifferenceFilterType =
    itk::Testing::ComparisonImageFilter<FFTInverseFilterType::OutputImageType, FFTInverseFilterType::OutputImageType>;
  auto differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  for (unsigned int c = 0; c < numComponents; c++)
  {
    vectorCastFilter->SetIndex(c);
    vectorCastFilter->Update();
    differenceFilter->SetValidInput(fftInverseFilter->GetOutput());
    differenceFilter->SetTestInput(vectorCastFilter->GetOutput());
    differenceFilter->Update();
    unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
    if (numberOfDiffPixels > 0)
    {
      std::cerr << "Test failed! " << std::endl;
      std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels << "unequal pixels" << std::endl;
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}
