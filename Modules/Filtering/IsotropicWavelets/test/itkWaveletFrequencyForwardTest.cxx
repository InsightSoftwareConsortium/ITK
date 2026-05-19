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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkNumberToString.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

std::string
AppendToFilename(const std::string & filename, const std::string & appendix)
{
  std::size_t foundDot = filename.find_last_of('.');
  return filename.substr(0, foundDot) + appendix + filename.substr(foundDot);
}

template <unsigned int VDimension, typename TWaveletFunction>
int
runWaveletFrequencyForwardTest(const std::string &  inputImage,
                               const std::string &  outputImage,
                               const unsigned int & inputLevels,
                               const unsigned int & inputBands)
{
  bool               testPassed = true;
  const unsigned int Dimension = VDimension;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  // Perform FFT on input image.
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());

  using ComplexImageType = typename FFTFilterType::OutputImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  using WaveletFunctionType = TWaveletFunction;
  using WaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>;
  using ForwardWaveletType = itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType>;

  unsigned int highSubBands = inputBands;
  unsigned int levels = inputLevels;

  auto forwardWavelet = ForwardWaveletType::New();
  forwardWavelet->SetHighPassSubBands(highSubBands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftFilter->GetOutput());
  auto waveletInstance = forwardWavelet->GetModifiableWaveletFunction();
  waveletInstance->Print(std::cout);
  forwardWavelet->Update();

  // Regression tests
  using OutputsType = typename ForwardWaveletType::OutputsType;
  OutputsType  allOutputs = forwardWavelet->GetOutputs();
  unsigned int expectedNumberOfOutputs = forwardWavelet->GetTotalOutputs();
  unsigned int computedNumberOfOutputs = forwardWavelet->GetOutputs().size();
  if (computedNumberOfOutputs != expectedNumberOfOutputs)
  {
    std::cerr << "Error in GetTotalOutputs()" << std::endl;
    std::cerr << "Expected: " << expectedNumberOfOutputs << ", but got " << computedNumberOfOutputs << std::endl;
    testPassed = false;
  }

  typename ForwardWaveletType::OutputImagePointer lowPass = forwardWavelet->GetOutputLowPass();

  OutputsType  allHighSubBands = forwardWavelet->GetOutputsHighPass();
  unsigned int expectedNumberOfHighSubBands = forwardWavelet->GetTotalOutputs() - 1;
  unsigned int computedNumberOfHighSubBands = forwardWavelet->GetOutputsHighPass().size();
  if (computedNumberOfHighSubBands != expectedNumberOfHighSubBands)
  {
    std::cerr << "Error in GetOutputsHighPass()" << std::endl;
    std::cerr << "Expected: " << expectedNumberOfHighSubBands << ", but got " << computedNumberOfHighSubBands
              << std::endl;
    testPassed = false;
  }

  OutputsType  highSubBandsPerLevel = forwardWavelet->GetOutputsHighPassByLevel(0);
  unsigned int expectedNumberOfHighSubBandsPerLevel = forwardWavelet->GetHighPassSubBands();
  unsigned int computedNumberOfHighSubBandsPerLevel = forwardWavelet->GetOutputsHighPassByLevel(0).size();
  if (computedNumberOfHighSubBandsPerLevel != expectedNumberOfHighSubBandsPerLevel)
  {
    std::cerr << "Error in GetOutputsHighPassByLevel()" << std::endl;
    std::cerr << "Expected: " << expectedNumberOfHighSubBandsPerLevel << ", but got "
              << computedNumberOfHighSubBandsPerLevel << std::endl;
    testPassed = false;
  }
  for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(i);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    std::cout << "InputIndex: " << i << " --> lv:" << lv << " b:" << b << std::endl;
  }

  // Inverse FFT Transform (Multilevel)
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();

  using WriterType = itk::ImageFileWriter<typename InverseFFTFilterType::OutputImageType>;
  auto writer = WriterType::New();

  typename ComplexImageType::SpacingType inputSpacing;
  inputSpacing.Fill(1.0);
  typename ComplexImageType::SpacingType expectedSpacing = inputSpacing;
  typename ComplexImageType::PointType   inputOrigin;
  inputOrigin.Fill(0.0);
  typename ComplexImageType::PointType expectedOrigin = inputOrigin;
  typename ComplexImageType::SizeType  inputSize = fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize();
  typename ComplexImageType::SizeType  expectedSize = inputSize;
  itk::NumberToString<unsigned int>    n2s;
  for (unsigned int level = 0; level < levels + 1; ++level)
  {
    double scaleFactorPerLevel =
      std::pow(static_cast<double>(forwardWavelet->GetScaleFactor()), static_cast<double>(level));
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      expectedSize[i] = inputSize[i] / scaleFactorPerLevel;
      expectedOrigin[i] = inputOrigin[i];
      expectedSpacing[i] = inputSpacing[i] * scaleFactorPerLevel;
    }
    for (unsigned int band = 0; band < highSubBands; ++band)
    {
      bool sizeIsCorrect = true;
      bool spacingIsCorrect = true;
      bool originIsCorrect = true;

      unsigned int nOutput = level * forwardWavelet->GetHighPassSubBands() + band;

      // Do not compute bands in low-pass level.
      if (level == levels && band == 0)
      {
        nOutput = forwardWavelet->GetTotalOutputs() - 1;
      }
      else if (level == levels && band != 0)
      {
        break;
      }

      if (expectedSize != forwardWavelet->GetOutput(nOutput)->GetLargestPossibleRegion().GetSize())
      {
        std::cerr << "Size of the output is not as expected: " << expectedSize << std::endl;
        sizeIsCorrect = false;
      }
      if (expectedOrigin != forwardWavelet->GetOutput(nOutput)->GetOrigin())
      {
        std::cerr << "Origin of the output is not as expected: " << expectedOrigin << std::endl;
        originIsCorrect = false;
      }
      if (expectedSpacing != forwardWavelet->GetOutput(nOutput)->GetSpacing())
      {
        std::cerr << "Spacing of the output is not as expected: " << expectedSpacing << std::endl;
        spacingIsCorrect = false;
      }

      if (!sizeIsCorrect || !originIsCorrect || !spacingIsCorrect)
      {
        testPassed = false;
        std::cerr << "OutputIndex : " << nOutput << std::endl;
        std::cerr << "Level: " << level << " / " << forwardWavelet->GetLevels() << std::endl;
        std::cerr << "Band: " << band << " / " << forwardWavelet->GetHighPassSubBands() << std::endl;
        // std::cerr << "Largest Region: " << forwardWavelet->GetOutput( nOutput )->GetLargestPossibleRegion() <<
        // std::endl;
        std::cerr << "Origin: " << forwardWavelet->GetOutput(nOutput)->GetOrigin() << std::endl;
        std::cerr << "Spacing: " << forwardWavelet->GetOutput(nOutput)->GetSpacing() << std::endl;
        std::cerr << "RegionSize: " << forwardWavelet->GetOutput(nOutput)->GetLargestPossibleRegion().GetSize()
                  << std::endl;
      }

      inverseFFT->SetInput(forwardWavelet->GetOutput(nOutput));
      inverseFFT->Update();

#ifdef ITK_VISUALIZE_TESTS
      std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(nOutput);
      itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(),
                                      "Wavelet coef. n_out: " + n2s(nOutput) + " level: " + n2s(pairLvBand.first) +
                                        " , band: " + n2s(pairLvBand.second) + "/" + n2s(inputBands));
#endif

      writer->SetFileName(AppendToFilename(outputImage, n2s(nOutput)));
      writer->SetInput(inverseFFT->GetOutput());
      ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
    }
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

int
itkWaveletFrequencyForwardTest(int argc, char * argv[])
{
  if (argc < 6 || argc > 7)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputLevels inputBands waveletFunction [dimension]"
              << std::endl;
    return EXIT_FAILURE;
  }
  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = std::stoi(argv[3]);
  const unsigned int inputBands = std::stoi(argv[4]);
  const std::string  waveletFunction = argv[5];

  unsigned int dimension = 3;
  if (argc == 7)
  {
    dimension = std::stoi(argv[6]);
  }

  constexpr unsigned int ImageDimension = 3;
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
  ITK_EXERCISE_BASIC_OBJECT_METHODS(heldIsotropicWavelet, HeldIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto vowIsotropicWavelet = VowIsotropicWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(vowIsotropicWavelet, VowIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto simoncellidIsotropicWavelet = SimoncelliIsotropicWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    simoncellidIsotropicWavelet, SimoncelliIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  auto shannonIsotropicWavelet = ShannonIsotropicWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    shannonIsotropicWavelet, ShannonIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  using HeldWavelet = itk::HeldIsotropicWavelet<>;
  using VowWavelet = itk::VowIsotropicWavelet<>;
  using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<>;
  using ShannonWavelet = itk::ShannonIsotropicWavelet<>;

  using HeldWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, HeldWavelet>;
  using VowWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, VowWavelet>;
  using SimoncelliWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, SimoncelliWavelet>;
  using ShannonWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, ShannonWavelet>;

  using HeldForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, HeldWaveletFilterBankType>;
  auto heldForwardWavelet = HeldForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(heldForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using VowForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>;
  auto vowForwardWavelet = VowForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(vowForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using SimoncelliForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>;
  auto simoncelliForwardWavelet = SimoncelliForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(simoncelliForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using ShannonForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>;
  auto shannonForwardWavelet = ShannonForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(shannonForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyForwardTest<2, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyForwardTest<2, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyForwardTest<2, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyForwardTest<2, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << argv[5] << " wavelet type not supported." << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyForwardTest<3, HeldWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyForwardTest<3, VowWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyForwardTest<3, SimoncelliWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyForwardTest<3, ShannonWavelet>(inputImage, outputImage, inputLevels, inputBands);
    }
    else
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << argv[5] << " wavelet type not supported." << std::endl;
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
