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
runWaveletFrequencyFilterBankGeneratorDownsampleTest(const std::string &  inputImage,
                                                     const std::string &  outputImage,
                                                     const unsigned int & inputLevels,
                                                     const unsigned int & inputBands)
{
  const unsigned int                       Dimension = VDimension;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  typedef TWaveletFunction                                                                WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType> WaveletFilterBankType;

  typename WaveletFilterBankType::Pointer forwardFilterBank = WaveletFilterBankType::New();
  unsigned int                            highSubBands = inputBands;
  forwardFilterBank->SetHighPassSubBands(highSubBands);
  typename ComplexImageType::SizeType inputSize = fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize();
  forwardFilterBank->SetSize(inputSize);
  forwardFilterBank->Update();
  // forwardFilterBank->Print(std::cout);

  // Test difference between downsample the filterbank output, or downsample the input and re-apply the filter bank.
  unsigned int shrinkFactor = 2;
  // typedef itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType> ShrinkFilterType;
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkFilterType;
  typename ShrinkFilterType::Pointer                        shrinkFilter = ShrinkFilterType::New();
  // shrinkFilter->SetInput(forwardFilterBank->GetOutputHighPass());
  shrinkFilter->SetInput(forwardFilterBank->GetOutputLowPass());
  shrinkFilter->SetShrinkFactors(shrinkFactor);
  shrinkFilter->Update();

  typename WaveletFilterBankType::Pointer forwardFilterBankDown = WaveletFilterBankType::New();
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
  typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  typename ComplexToRealFilter::Pointer                              complexToRealFilter = ComplexToRealFilter::New();
  complexToRealFilter->SetInput(shrinkFilter->GetOutput());
  complexToRealFilter->Update();
  itk::Testing::ViewImage(complexToRealFilter->GetOutput(), "shrinked (by half) FilterBank");
  // complexToRealFilter->SetInput(forwardFilterBankDown->GetOutputHighPass());
  complexToRealFilter->SetInput(forwardFilterBankDown->GetOutputLowPass());
  complexToRealFilter->Update();
  itk::Testing::ViewImage(complexToRealFilter->GetOutput(), "FilterBank of halfSizeImage (highPassBand)");
#endif

  return EXIT_SUCCESS;
}

int
itkWaveletFrequencyFilterBankGeneratorDownsampleTest(int argc, char * argv[])
{
  if (argc < 6 || argc > 7)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage levels inputBands waveletFunction [dimension]"
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

  const unsigned int                                   ImageDimension = 2;
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

  HeldWaveletFilterBankType::Pointer heldWaveletFilterBankGenerator = HeldWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    heldWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  VowWaveletFilterBankType::Pointer vowWaveletFilterBankGenerator = VowWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    vowWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  SimoncelliWaveletFilterBankType::Pointer simoncelliWaveletFilterBankGenerator =
    SimoncelliWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    simoncelliWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);

  ShannonWaveletFilterBankType::Pointer shannonWaveletFilterBankGenerator = ShannonWaveletFilterBankType::New();
  EXERCISE_BASIC_OBJECT_METHODS(
    shannonWaveletFilterBankGenerator, WaveletFrequencyFilterBankGenerator, GenerateImageSource);


  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<2, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
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
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyFilterBankGeneratorDownsampleTest<3, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands);
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
