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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkFrequencyShrinkImageFilter.h"
#include "itkFrequencyShrinkViaInverseFFTImageFilter.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumberToString.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension, typename TWaveletFunction>
int
runWaveletFrequencyFilterBankGeneratorDownsampleTest(const std::string & inputImage,
                                                     const std::string &,
                                                     const unsigned int & inputBands)
{
  const unsigned int Dimension = VDimension;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();
  using ComplexImageType = typename FFTFilterType::OutputImageType;

  using WaveletFunctionType = TWaveletFunction;
  using WaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>;

  auto         forwardFilterBank = WaveletFilterBankType::New();
  unsigned int highSubBands = inputBands;
  forwardFilterBank->SetHighPassSubBands(highSubBands);
  typename ComplexImageType::SizeType inputSize = fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize();
  forwardFilterBank->SetSize(inputSize);
  forwardFilterBank->Update();
  // forwardFilterBank->Print(std::cout);

  // Test difference between downsample the filterbank output, or downsample the input and re-apply the filter bank.
  unsigned int shrinkFactor = 2;
  // using ShrinkFilterType = itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType>;
  using ShrinkFilterType = itk::FrequencyShrinkImageFilter<ComplexImageType>;
  auto shrinkFilter = ShrinkFilterType::New();
  // shrinkFilter->SetInput(forwardFilterBank->GetOutputHighPass());
  shrinkFilter->SetInput(forwardFilterBank->GetOutputLowPass());
  shrinkFilter->SetShrinkFactors(shrinkFactor);
  shrinkFilter->Update();

  auto forwardFilterBankDown = WaveletFilterBankType::New();
  forwardFilterBankDown->SetHighPassSubBands(highSubBands);
  typename ComplexImageType::SizeType halfSize;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    halfSize[i] = itk::Math::Floor<itk::SizeValueType>(inputSize[i] / static_cast<double>(shrinkFactor));
  }
  forwardFilterBankDown->SetSize(halfSize);
  forwardFilterBankDown->Update();

  // Compare Images
#ifdef ITK_VISUALIZE_TESTS
  using ComplexToRealFilter = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  auto complexToRealFilter = ComplexToRealFilter::New();
  complexToRealFilter->SetInput(shrinkFilter->GetOutput());
  complexToRealFilter->Update();
  itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(), "shrinked (by half) FilterBank");
  // complexToRealFilter->SetInput(forwardFilterBankDown->GetOutputHighPass());
  complexToRealFilter->SetInput(forwardFilterBankDown->GetOutputLowPass());
  complexToRealFilter->Update();
  itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(), "FilterBank of halfSizeImage (highPassBand)");
#endif

  return EXIT_SUCCESS;
}

int
itkWaveletFrequencyFilterBankGeneratorDownsampleTest(int argc, char * argv[])
{
  if (argc < 5 || argc > 6)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputBands waveletFunction [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputBands = std::stoi(argv[3]);
  const std::string  waveletFunction = argv[4];

  unsigned int dimension = 3;
  if (argc == 6)
  {
    dimension = std::stoi(argv[5]);
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = double;
  using ComplexPixelType = std::complex<PixelType>;
  using PointType = itk::Point<PixelType, ImageDimension>;
  using ComplexImageType = itk::Image<ComplexPixelType, ImageDimension>;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using HeldIsotropicWaveletType = itk::HeldIsotropicWavelet<PixelType, ImageDimension, PointType>;
  using VowIsotropicWaveletType = itk::VowIsotropicWavelet<PixelType, ImageDimension, PointType>;
  using SimoncelliIsotropicWaveletType = itk::SimoncelliIsotropicWavelet<PixelType, ImageDimension, PointType>;
  using ShannonIsotropicWaveletType = itk::ShannonIsotropicWavelet<PixelType, ImageDimension, PointType>;

  auto heldIsotropicWavelet = HeldIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(heldIsotropicWavelet, HeldIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto vowIsotropicWavelet = VowIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(vowIsotropicWavelet, VowIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto simoncellidIsotropicWavelet = SimoncelliIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    simoncellidIsotropicWavelet, SimoncelliIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto shannonIsotropicWavelet = ShannonIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(shannonIsotropicWavelet, ShannonIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  using HeldWavelet = itk::HeldIsotropicWavelet<>;
  using VowWavelet = itk::VowIsotropicWavelet<>;
  using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<>;
  using ShannonWavelet = itk::ShannonIsotropicWavelet<>;

  using HeldWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, HeldWavelet>;
  using VowWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, VowWavelet>;
  using SimoncelliWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, SimoncelliWavelet>;
  using ShannonWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, ShannonWavelet>;

  auto heldWaveletFilterBankGenerator = HeldWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    heldWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  auto vowWaveletFilterBankGenerator = VowWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    vowWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  auto simoncelliWaveletFilterBankGenerator = SimoncelliWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    simoncelliWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  auto shannonWaveletFilterBankGenerator = ShannonWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    shannonWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, HeldWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, VowWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, ShannonWavelet>(
        inputImage, outputImage, inputBands);
    }
    else
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << argv[4] << " wavelet type not supported." << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, HeldWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, VowWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, ShannonWavelet>(
        inputImage, outputImage, inputBands);
    }
    else
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << argv[4] << " wavelet type not supported." << std::endl;
      return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
