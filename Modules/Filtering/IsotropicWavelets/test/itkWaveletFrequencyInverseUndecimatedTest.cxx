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
#include "itkWaveletFrequencyForwardUndecimated.h"
#include "itkWaveletFrequencyInverseUndecimated.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

#ifdef ITK_VISUALIZE_TESTS
#  include "itkComplexToRealImageFilter.h"
#  include "itkNumberToString.h"
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension, typename TWaveletFunction>
int
runWaveletFrequencyInverseUndecimatedTest(const std::string &  inputImage,
                                          const std::string &  outputImage,
                                          const unsigned int & inputLevels,
                                          const unsigned int & inputBands,
                                          bool                 inputUseWaveletFilterBankPyramid)
{
  bool               testPassed = true;
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

  using ComplexImageType = typename FFTFilterType::OutputImageType;

  // Set the WaveletFunctionType and the WaveletFilterBank
  using WaveletFunctionType = TWaveletFunction;
  using WaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>;
  using ForwardWaveletType =
    itk::WaveletFrequencyForwardUndecimated<ComplexImageType, ComplexImageType, WaveletFilterBankType>;

  auto forwardWavelet = ForwardWaveletType::New();

  forwardWavelet->SetHighPassSubBands(inputBands);
  forwardWavelet->SetLevels(inputLevels);
  forwardWavelet->SetInput(fftFilter->GetOutput());
  forwardWavelet->StoreWaveletFilterBankPyramidOn();
  forwardWavelet->Update();

  unsigned int noutputs = forwardWavelet->GetNumberOfOutputs();

  std::cout << "Number of inputs: " << noutputs << std::endl;
  for (unsigned int i = 0; i < noutputs; ++i)
  {
    std::cout << "Input number: " << i << std::endl;
    std::cout << "Region: " << forwardWavelet->GetOutput(i)->GetLargestPossibleRegion() << std::endl;
    std::cout << "Spacing: " << forwardWavelet->GetOutput(i)->GetSpacing() << std::endl;
  }

  // Inverse Wavelet Transform
  using InverseWaveletType =
    itk::WaveletFrequencyInverseUndecimated<ComplexImageType, ComplexImageType, WaveletFilterBankType>;
  auto inverseWavelet = InverseWaveletType::New();

  inverseWavelet->SetHighPassSubBands(inputBands);
  inverseWavelet->SetLevels(inputLevels);
  inverseWavelet->SetInputs(forwardWavelet->GetOutputs());
  inverseWavelet->SetUseWaveletFilterBankPyramid(inputUseWaveletFilterBankPyramid);
  inverseWavelet->SetWaveletFilterBankPyramid(forwardWavelet->GetWaveletFilterBankPyramid());
  inverseWavelet->DebugOn();
  inverseWavelet->Update();

  // Check Metadata: Spacing, Origin
  typename ComplexImageType::SpacingType outputSpacing = inverseWavelet->GetOutput()->GetSpacing();
  typename ComplexImageType::SpacingType expectedSpacing;
  expectedSpacing.Fill(1.0);
  typename ComplexImageType::PointType outputOrigin = inverseWavelet->GetOutput()->GetOrigin();
  typename ComplexImageType::PointType expectedOrigin;
  expectedOrigin.Fill(0.0);
  typename ComplexImageType::SizeType outputSize = inverseWavelet->GetOutput()->GetLargestPossibleRegion().GetSize();
  typename ComplexImageType::SizeType expectedSize = fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize();

  if (outputSpacing != expectedSpacing)
  {
    std::cout << "outputSpacing is wrong: " << outputSpacing << " expectedSpacing: " << expectedSpacing << std::endl;
    testPassed = false;
  }
  if (outputOrigin != expectedOrigin)
  {
    std::cout << "outputOrigin is wrong: " << outputOrigin << " expectedOrigin: " << expectedOrigin << std::endl;
    testPassed = false;
  }
  if (outputSize != expectedSize)
  {
    std::cout << "outputSize is wrong: " << outputSize << " expectedSize: " << expectedSize << std::endl;
    testPassed = false;
  }

  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(inverseWavelet->GetOutput());
  inverseFFT->Update();

  // Write output image
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(inverseFFT->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(reader->GetOutput(), "Original");
  itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "InverseWavelet");
#endif

  // TODO move it from here to Forward test.
#ifdef ITK_VISUALIZE_TESTS
  std::vector<typename ComplexImageType::Pointer> waveletFilterBankPyramid =
    forwardWavelet->GetWaveletFilterBankPyramid();
  using ComplexToRealFilterType = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  auto complexToRealFilter = ComplexToRealFilterType::New();

  itk::NumberToString<unsigned int> n2s;
  std::cout << "Size FilterBankPyramid:" << waveletFilterBankPyramid.size() << std::endl;
  for (unsigned int i = 0; i < waveletFilterBankPyramid.size(); ++i)
  {
    complexToRealFilter->SetInput(waveletFilterBankPyramid[i]);
    complexToRealFilter->UpdateLargestPossibleRegion();
    itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(), "FilterBankPyramid #" + n2s(i));
  }
#endif

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
itkWaveletFrequencyInverseUndecimatedTest(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Usage: " << argv[0]
              << " inputImage outputImage inputLevels inputBands waveletFunction "
                 "reuseFilterBankPyramid|noFilterBankPyramid dimension"
              << std::endl;
    return EXIT_FAILURE;
  }

  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = std::stoi(argv[3]);
  const unsigned int inputBands = std::stoi(argv[4]);
  const std::string  waveletFunction = argv[5];
  const std::string  inputUseWaveletFilterBankPyramid = argv[6];
  bool               useWaveletFilterBankPyramid = false;
  if (inputUseWaveletFilterBankPyramid == "reuseFilterBankPyramid")
  {
    useWaveletFilterBankPyramid = true;
  }
  else if (inputUseWaveletFilterBankPyramid == "noFilterBankPyramid")
  {
    useWaveletFilterBankPyramid = false;
  }

  unsigned int dimension = std::stoi(argv[7]);

  // const unsigned int ImageDimension = 3;
  // using PixelType = double;
  // using ComplexPixelType = std::complex< PixelType >;
  // using PointType = itk::Point< PixelType, ImageDimension >;
  // using ComplexImageType = itk::Image< ComplexPixelType, ImageDimension >;

  // // Exercise basic object methods
  // // Done outside the helper function in the test because GCC is limited
  // // when calling overloaded base class functions.
  // using HeldIsotropicWaveletType = itk::HeldIsotropicWavelet< PixelType, ImageDimension, PointType >;
  // using VowIsotropicWaveletType = itk::VowIsotropicWavelet< PixelType, ImageDimension, PointType >;
  // using SimoncelliIsotropicWaveletType = itk::SimoncelliIsotropicWavelet< PixelType, ImageDimension, PointType >;
  // using ShannonIsotropicWaveletType = itk::ShannonIsotropicWavelet< PixelType, ImageDimension, PointType >;
  //
  // auto heldIsotropicWavelet = HeldIsotropicWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( heldIsotropicWavelet, HeldIsotropicWavelet,
  //   IsotropicWaveletFrequencyFunction );
  //
  // auto vowIsotropicWavelet = VowIsotropicWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( vowIsotropicWavelet, VowIsotropicWavelet,
  //   IsotropicWaveletFrequencyFunction );
  //
  // auto simoncellidIsotropicWavelet = SimoncelliIsotropicWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( simoncellidIsotropicWavelet, SimoncelliIsotropicWavelet,
  //   IsotropicWaveletFrequencyFunction );
  //
  // auto shannonIsotropicWavelet = ShannonIsotropicWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( shannonIsotropicWavelet, ShannonIsotropicWavelet,
  //   IsotropicWaveletFrequencyFunction );
  //
  //
  using HeldWavelet = itk::HeldIsotropicWavelet<>;
  using VowWavelet = itk::VowIsotropicWavelet<>;
  using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<>;
  using ShannonWavelet = itk::ShannonIsotropicWavelet<>;
  //
  //
  // using HeldWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator< ComplexImageType, HeldWavelet >;
  // using VowWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator< ComplexImageType, VowWavelet >;
  // using SimoncelliWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator< ComplexImageType,
  // SimoncelliWavelet >; using ShannonWaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<
  // ComplexImageType, ShannonWavelet >;
  //
  // using HeldInverseWaveletType = itk::WaveletFrequencyInverseUndecimated< ComplexImageType, ComplexImageType,
  // HeldWaveletFilterBankType >; auto heldInverseWavelet = HeldInverseWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( heldInverseWavelet, WaveletFrequencyInverseUndecimated,
  //   ImageToImageFilter );
  //
  // using VowInverseWaveletType = itk::WaveletFrequencyInverseUndecimated< ComplexImageType, ComplexImageType,
  // VowWaveletFilterBankType >; auto vowInverseWavelet = VowInverseWaveletType::New(); EXERCISE_BASIC_OBJECT_METHODS(
  // vowInverseWavelet, WaveletFrequencyInverseUndecimated,
  //   ImageToImageFilter );
  //
  // using SimoncelliInverseWaveletType = itk::WaveletFrequencyInverseUndecimated< ComplexImageType, ComplexImageType,
  // SimoncelliWaveletFilterBankType >; auto simoncelliInverseWavelet = SimoncelliInverseWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( simoncelliInverseWavelet, WaveletFrequencyInverseUndecimated,
  //   ImageToImageFilter );
  //
  // using ShannonInverseWaveletType = itk::WaveletFrequencyInverseUndecimated< ComplexImageType, ComplexImageType,
  // ShannonWaveletFilterBankType >; auto shannonInverseWavelet = ShannonInverseWaveletType::New();
  // EXERCISE_BASIC_OBJECT_METHODS( shannonInverseWavelet, WaveletFrequencyInverseUndecimated,
  //   ImageToImageFilter );

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runWaveletFrequencyInverseUndecimatedTest<2, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyInverseUndecimatedTest<2, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyInverseUndecimatedTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyInverseUndecimatedTest<2, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
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
      return runWaveletFrequencyInverseUndecimatedTest<3, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Vow")
    {
      return runWaveletFrequencyInverseUndecimatedTest<3, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runWaveletFrequencyInverseUndecimatedTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
    }
    else if (waveletFunction == "Shannon")
    {
      return runWaveletFrequencyInverseUndecimatedTest<3, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, useWaveletFilterBankPyramid);
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
