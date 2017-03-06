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

  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());

  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  typedef TWaveletFunction                                                                        WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>         WaveletFilterBankType;
  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType> ForwardWaveletType;

  unsigned int highSubBands = inputBands;
  unsigned int levels = inputLevels;

  typename ForwardWaveletType::Pointer forwardWavelet = ForwardWaveletType::New();
  forwardWavelet->SetHighPassSubBands(highSubBands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftFilter->GetOutput());
  forwardWavelet->Update();

  // Regression tests
  typedef typename ForwardWaveletType::OutputsType OutputsType;
  OutputsType                                      allOutputs = forwardWavelet->GetOutputs();
  unsigned int                                     expectedNumberOfOutputs = forwardWavelet->GetTotalOutputs();
  unsigned int                                     computedNumberOfOutputs = forwardWavelet->GetOutputs().size();
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

  // Test OutputIndexToLevelBand
  bool testOutputIndexToLevelBandPassed = true;
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(0);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    if (lv != 0 || b != 0)
    {
      std::cerr << "Error in first index: inputindex: " << 0 << " lv:" << lv << " b:" << b << " should be (0, 0)"
                << std::endl;
      testOutputIndexToLevelBandPassed = false;
    }
  }
  {
    std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(highSubBands);
    unsigned int                          lv = pairLvBand.first;
    unsigned int                          b = pairLvBand.second;
    if (lv != 1 || b != 0)
    {
      std::cerr << "Error in inputindex: " << highSubBands << " lv:" << lv << " b:" << b << " should be (1, 0)"
                << std::endl;
      testOutputIndexToLevelBandPassed = false;
    }
  }
  {
    std::pair<unsigned int, unsigned int> pairLvBand =
      forwardWavelet->OutputIndexToLevelBand(forwardWavelet->GetTotalOutputs() - 1);
    unsigned int lv = pairLvBand.first;
    unsigned int b = pairLvBand.second;
    if (lv != levels || b != 0)
    {
      std::cerr << "Error in last inputindex: " << highSubBands << " lv:" << lv << " b:" << b << " should be ("
                << levels << ", 0)" << std::endl;
      testOutputIndexToLevelBandPassed = false;
    }
  }
  if (!testOutputIndexToLevelBandPassed)
  {
    std::cerr << "Error in OutputIndexToLevelBand" << std::endl;
    testPassed = false;
  }

  // Inverse FFT Transform (Multilevel)
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                          inverseFFT = InverseFFTFilterType::New();

  typedef itk::ImageFileWriter<typename InverseFFTFilterType::OutputImageType> WriterType;
  typename WriterType::Pointer                                                 writer = WriterType::New();

  typename ComplexImageType::SpacingType inputSpacing = { { 1.0 } };
  typename ComplexImageType::SpacingType expectedSpacing = inputSpacing;
  typename ComplexImageType::PointType   inputOrigin = { { 0.0 } };
  typename ComplexImageType::PointType   expectedOrigin = inputOrigin;
  typename ComplexImageType::SizeType    inputSize = fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize();
  typename ComplexImageType::SizeType    expectedSize = inputSize;
  itk::NumberToString<unsigned int>      n2s;
  for (unsigned int level = 0; level < levels; ++level)
  {
    double       scaleFactorPerLevel = std::pow(static_cast<double>(forwardWavelet->GetScaleFactor()), level + 1);
    unsigned int nOutput = 0;
    for (unsigned int band = 0; band < highSubBands; ++band)
    {
      bool sizeIsCorrect = true;
      bool spacingIsCorrect = true;
      bool originIsCorrect = true;

      if (level == 0 && band == 0) // Low pass
      {
        nOutput = 0;
        scaleFactorPerLevel = std::pow(static_cast<double>(forwardWavelet->GetScaleFactor()), levels);
      }
      else
      {
        nOutput = 1 + level * forwardWavelet->GetHighPassSubBands() + band;
        scaleFactorPerLevel = std::pow(static_cast<double>(forwardWavelet->GetScaleFactor()), level + 1);
      }

      for (unsigned int i = 0; i < Dimension; ++i)
      {
        expectedSize[i] = inputSize[i] / scaleFactorPerLevel;
        expectedOrigin[i] = inputOrigin[i];
        expectedSpacing[i] = inputSpacing[i] / scaleFactorPerLevel;
      }
      if (expectedSize != forwardWavelet->GetOutput(nOutput)->GetLargestPossibleRegion().GetSize())
      {
        std::cerr << "Size of the output is not as expected." << std::endl;
        sizeIsCorrect = false;
      }
      if (expectedOrigin != forwardWavelet->GetOutput(nOutput)->GetOrigin())
      {
        std::cerr << "Origin of the output is not as expected." << std::endl;
        originIsCorrect = false;
      }
      if (expectedSpacing != forwardWavelet->GetOutput(nOutput)->GetSpacing())
      {
        std::cerr << "Spacing of the output is not as expected." << std::endl;
        spacingIsCorrect = false;
      }

      if (!sizeIsCorrect || !originIsCorrect || !spacingIsCorrect)
      {
        testPassed = false;
        std::cerr << "OutputIndex : " << nOutput << std::endl;
        std::cerr << "Level: " << level + 1 << " / " << forwardWavelet->GetLevels() << std::endl;
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
      if (level == 0 && band == 0) // Low pass
      {
        itk::Testing::ViewImage(inverseFFT->GetOutput(),
                                "Approx coef. n_out: " + n2s(nOutput) + " level: " + n2s(levels) + ", band:0/" +
                                  n2s(inputBands));
      }
      else
      {
        std::pair<unsigned int, unsigned int> pairLvBand = forwardWavelet->OutputIndexToLevelBand(nOutput);
        itk::Testing::ViewImage(inverseFFT->GetOutput(),
                                "Wavelet coef. n_out: " + n2s(nOutput) + " level: " + n2s(pairLvBand.first) +
                                  " , band: " + n2s(pairLvBand.second) + "/" + n2s(inputBands));
      }
#endif

      writer->SetFileName(AppendToFilename(outputImage, n2s(nOutput)));
      writer->SetInput(inverseFFT->GetOutput());
      TRY_EXPECT_NO_EXCEPTION(writer->Update());
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
  const unsigned int inputLevels = atoi(argv[3]);
  const unsigned int inputBands = atoi(argv[4]);
  const std::string  waveletFunction = argv[5];

  unsigned int dimension = 3;
  if (argc == 7)
  {
    dimension = atoi(argv[6]);
  }

  const unsigned int                                   ImageDimension = 3;
  typedef double                                       PixelType;
  typedef std::complex<PixelType>                      ComplexPixelType;
  typedef itk::Point<PixelType, ImageDimension>        PointType;
  typedef itk::Image<ComplexPixelType, ImageDimension> ComplexImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::HeldIsotropicWavelet<PixelType, ImageDimension, PointType>       HeldIsotropicWaveletType;
  typedef itk::VowIsotropicWavelet<PixelType, ImageDimension, PointType>        VowIsotropicWaveletType;
  typedef itk::SimoncelliIsotropicWavelet<PixelType, ImageDimension, PointType> SimoncelliIsotropicWaveletType;
  typedef itk::ShannonIsotropicWavelet<PixelType, ImageDimension, PointType>    ShannonIsotropicWaveletType;

  HeldIsotropicWaveletType::Pointer heldIsotropicWavelet = HeldIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(heldIsotropicWavelet, HeldIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  VowIsotropicWaveletType::Pointer vowIsotropicWavelet = VowIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(vowIsotropicWavelet, VowIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  SimoncelliIsotropicWaveletType::Pointer simoncellidIsotropicWavelet = SimoncelliIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    simoncellidIsotropicWavelet, SimoncelliIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  ShannonIsotropicWaveletType::Pointer shannonIsotropicWavelet = ShannonIsotropicWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(shannonIsotropicWavelet, ShannonIsotropicWavelet, IsotropicWaveletFrequencyFunction);


  typedef itk::HeldIsotropicWavelet<>       HeldWavelet;
  typedef itk::VowIsotropicWavelet<>        VowWavelet;
  typedef itk::SimoncelliIsotropicWavelet<> SimoncelliWavelet;
  typedef itk::ShannonIsotropicWavelet<>    ShannonWavelet;

  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, HeldWavelet>       HeldWaveletFilterBankType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, VowWavelet>        VowWaveletFilterBankType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, SimoncelliWavelet> SimoncelliWaveletFilterBankType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, ShannonWavelet>    ShannonWaveletFilterBankType;

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, HeldWaveletFilterBankType>
                                  HeldForwardWaveletType;
  HeldForwardWaveletType::Pointer heldForwardWavelet = HeldForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(heldForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>
                                 VowForwardWaveletType;
  VowForwardWaveletType::Pointer vowForwardWavelet = VowForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(vowForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>
                                        SimoncelliForwardWaveletType;
  SimoncelliForwardWaveletType::Pointer simoncelliForwardWavelet = SimoncelliForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(simoncelliForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>
                                     ShannonForwardWaveletType;
  ShannonForwardWaveletType::Pointer shannonForwardWavelet = ShannonForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(shannonForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);


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
