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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
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
runWaveletFrequencyFilterBankGeneratorTest(const std::string &  inputImage,
                                           const std::string &  outputImage,
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
  forwardFilterBank->SetSize(fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize());

  // Test Get/Set
  TEST_SET_GET_VALUE(highSubBands, forwardFilterBank->GetHighPassSubBands());
  TEST_SET_GET_BOOLEAN(forwardFilterBank, InverseBank, false);
  auto waveletInstance = forwardFilterBank->GetModifiableWaveletFunction();
  waveletInstance->Print(std::cout);

  forwardFilterBank->Update();
  // forwardFilterBank->Print(std::cout);
  // Get real part of complex image for visualization
  using ComplexToRealFilter = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  auto complexToRealFilter = ComplexToRealFilter::New();

  std::cout << "Real part of complex image:" << std::endl;
  for (unsigned int i = 0; i < highSubBands + 1; ++i)
  {
    std::cout << "Band #: " << i << " / " << forwardFilterBank->GetHighPassSubBands() << std::endl;
    // std::cout << "Largest Region: " << forwardFilterBank->GetOutput(i)->GetLargestPossibleRegion() << std::endl;

    complexToRealFilter->SetInput(forwardFilterBank->GetOutput(i));
    complexToRealFilter->Update();

#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<unsigned int> n2s;
    itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(),
                                    "RealPart of Complex. Band: " + n2s(i) + "/" + n2s(highSubBands));
#endif
  }

  // Write only the last band
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(complexToRealFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Inverse FFT Transform
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();
  std::cout << "InverseFFT:" << std::endl;
  for (unsigned int i = 0; i < highSubBands + 1; ++i)
  {
    std::cout << "Band #: " << i << " / " << forwardFilterBank->GetHighPassSubBands() << std::endl;
    inverseFFT->SetInput(forwardFilterBank->GetOutput(i));
    inverseFFT->Update();

#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<unsigned int> n2s;
    itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "InverseFFT. Band: " + n2s(i) + "/" + n2s(highSubBands));
#endif
  }

  // Create a new filter for the inverse Filter Bank
  // If we just change the InverseFlag, the output already generated by the filter will get overriden, triggering the
  // pipeline.
  auto inverseFilterBank = WaveletFilterBankType::New();
  inverseFilterBank->SetInverseBank(true);
  inverseFilterBank->SetHighPassSubBands(highSubBands);
  inverseFilterBank->SetSize(fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize());
  inverseFilterBank->Update();

  // itk::Testing::ComparisonImageFilter does not work with complex images.
  using ComplexConstRegionIterator = itk::ImageRegionConstIterator<ComplexImageType>;
  unsigned int ne = 0;
  for (unsigned int i = 0; i < highSubBands + 1; ++i)
  {
    auto                       outForward = forwardFilterBank->GetOutput(i);
    auto                       outInverse = inverseFilterBank->GetOutput(i);
    ComplexConstRegionIterator itForward(outForward, outForward->GetLargestPossibleRegion());
    ComplexConstRegionIterator itInverse(outInverse, outInverse->GetLargestPossibleRegion());
    itForward.GoToBegin();
    itInverse.GoToBegin();
    unsigned int nePerBand = 0;
    while (!itForward.IsAtEnd() || !itInverse.IsAtEnd())
    {
      if (itForward.Get() != itInverse.Get())
      {
        ++nePerBand;
      }
      ++itForward;
      ++itInverse;
    }

    ne += nePerBand;
  }

  if (ne > 0)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Comparison error: number of errors: " << ne << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "No comparison errors: " << ne << " errors" << std::endl;
  }

  return EXIT_SUCCESS;
}

int
itkWaveletFrequencyFilterBankGeneratorTest(int argc, char * argv[])
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
      return runWaveletFrequencyFilterBankGeneratorTest<2, HeldWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<2, VowWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<2, SimoncelliWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<2, ShannonWavelet>(inputImage, outputImage, inputBands);
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
      return runWaveletFrequencyFilterBankGeneratorTest<3, HeldWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<3, VowWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<3, SimoncelliWavelet>(inputImage, outputImage, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorTest<3, ShannonWavelet>(inputImage, outputImage, inputBands);
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
