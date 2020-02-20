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

#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyInverse.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"

#include "itkRieszFrequencyFilterBankGenerator.h"
#include "itkStructureTensor.h"
#include "itkZeroDCImageFilter.h"
#include "itkMultiplyImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkTestingMacros.h"

#include <string>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#  include "itkNumberToString.h"
#endif

template <unsigned int VDimension, typename TWaveletFunction>
int
runStructureTensorWithGeneralizedRieszTest(const std::string & inputImage,
                                           const std::string &, // outputImage
                                           const unsigned int & inputLevels,
                                           const unsigned int & inputBands,
                                           const unsigned int & inputRieszOrder,
                                           const bool           inputApplyReconstructionFactors)
{
  const unsigned int Dimension = VDimension;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  using ZeroDCFilterType = itk::ZeroDCImageFilter<ImageType>;
  auto zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  zeroDCFilter->Update();

  // Perform FFT on input image.
  using FFTForwardFilterType = itk::ForwardFFTImageFilter<typename ZeroDCFilterType::OutputImageType>;
  auto fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(zeroDCFilter->GetOutput());
  fftForwardFilter->Update();
  using ComplexImageType = typename FFTForwardFilterType::OutputImageType;

  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;

  // Forward Wavelet
  using WaveletFunctionType = TWaveletFunction;
  using WaveletFilterBankType = itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>;
  using ForwardWaveletType = itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType>;
  auto         forwardWavelet = ForwardWaveletType::New();
  unsigned int highSubBands = inputBands;
  unsigned int levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(highSubBands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftForwardFilter->GetOutput());
  forwardWavelet->Update();
  typename ForwardWaveletType::OutputsType analysisWavelets = forwardWavelet->GetOutputs();

  // Generalized Riesz Function of Order N.
  // using FunctionType = itk::RieszFrequencyFunction<>;
  using RieszFilterBankType = itk::RieszFrequencyFilterBankGenerator<ComplexImageType>;
  // // Get iterator to Indices of RieszFunction.
  // using IndicesType = RieszFilterBankType::RieszFunctionType::SetType;
  // IndicesType indices = filterBank->GetModifiableEvaluator()->GetIndices();
  // IndicesType::const_iterator indicesIt = indices.begin();

  using MultiplyFilterType = itk::MultiplyImageFilter<ComplexImageType>;

  typename ForwardWaveletType::OutputsType modifiedWavelets;
  unsigned int                             numberOfOutputs = forwardWavelet->GetNumberOfOutputs();
  std::cout << "RieszOrder: " << inputRieszOrder << std::endl;
  for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
  {
    std::cout << "Output #: " << i << " / " << numberOfOutputs - 1 << std::endl;
    if (i == numberOfOutputs - 1) // Don't apply riesz stuff to the low pass.
    {
      modifiedWavelets.push_back(analysisWavelets[i]);
      continue;
    }

    auto filterBank = RieszFilterBankType::New();
    filterBank->SetOutputParametersFromImage(analysisWavelets[i]);
    filterBank->SetOrder(inputRieszOrder);
    filterBank->Update();
    std::cout << "RieszOutputs: " << filterBank->GetNumberOfOutputs() << std::endl;
    std::vector<typename ComplexImageType::Pointer> rieszOutputs = filterBank->GetOutputs();
    std::vector<typename ComplexImageType::Pointer> rieszWavelets;
    std::vector<typename ImageType::Pointer>        rieszWaveletsSpatial;
    for (unsigned int rieszComp = 0; rieszComp < filterBank->GetNumberOfOutputs(); ++rieszComp)
    {
      // Multiply wavelet with riesz.
      auto multiplyWaveletRiesz = MultiplyFilterType::New();
      multiplyWaveletRiesz->SetInput1(analysisWavelets[i]);
      multiplyWaveletRiesz->SetInput2(rieszOutputs[rieszComp]);
      multiplyWaveletRiesz->Update();
      rieszWavelets.push_back(multiplyWaveletRiesz->GetOutput());
      auto inverseFFT = InverseFFTFilterType::New();
      inverseFFT->SetInput(rieszWavelets[rieszComp]);
      inverseFFT->Update();
      rieszWaveletsSpatial.push_back(inverseFFT->GetOutput());
#ifdef ITK_VISUALIZE_TESTS
      bool visualizeRieszWavelets = true;
      if (visualizeRieszWavelets)
      {
        itk::NumberToString<unsigned int> n2s;
        itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(),
                                        "RieszWaveletCoef: output #" + n2s(i) + " RieszComp: " + n2s(rieszComp));
      }
      bool visualizeRieszWaveletsInFrequency = false;
      if (visualizeRieszWaveletsInFrequency)
      {
        itk::NumberToString<unsigned int> n2s;
        using ComplexToRealFilterType = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
        using ComplexToImaginaryFilterType = itk::ComplexToImaginaryImageFilter<ComplexImageType, ImageType>;
        auto complexToReal = ComplexToRealFilterType::New();
        auto complexToImaginary = ComplexToImaginaryFilterType::New();
        complexToReal->SetInput(rieszWavelets[rieszComp]);
        complexToReal->Update();
        itk::ViewImage<ImageType>::View(complexToReal->GetOutput(),
                                        "REAL:RieszWaveletCoef: output #" + n2s(i) + " RieszComp: " + n2s(rieszComp));
        complexToImaginary->SetInput(rieszWavelets[rieszComp]);
        complexToImaginary->Update();
        itk::ViewImage<ImageType>::View(complexToImaginary->GetOutput(),
                                        "IMAGINARY:RieszWaveletCoef: output #" + n2s(i) +
                                          " RieszComp: " + n2s(rieszComp));
      }
#endif
    }

    // Structure Tensor
    using StructureTensorType = itk::StructureTensor<ImageType>;
    auto tensor = StructureTensorType::New();
    tensor->SetInputs(rieszWaveletsSpatial);
    // tensor->SetGaussianWindowRadius(3);
    tensor->Update();
    auto fftForwardTensor = FFTForwardFilterType::New();
    fftForwardTensor->SetInput(tensor->ComputeProjectionImageWithLargestResponse());
    fftForwardTensor->Update();

    modifiedWavelets.push_back(fftForwardTensor->GetOutput());
    modifiedWavelets.back()->DisconnectPipeline();
  }

#ifdef ITK_VISUALIZE_TESTS
  // Visualize and compare modified wavelets coefficients (and approx image)
  bool visualizeCoefficients = true;
  if (visualizeCoefficients)
  {
    for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
    {
      itk::NumberToString<unsigned int> n2s;
      auto                              inverseFFT = InverseFFTFilterType::New();
      inverseFFT->SetInput(analysisWavelets[i]);
      inverseFFT->Update();
      itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "WaveletCoef: output #" + n2s(i));
      inverseFFT->SetInput(modifiedWavelets[i]);
      inverseFFT->Update();
      itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(),
                                      "WaveletCoef. LargestComponentStructureTensor #" + n2s(i));
    }
  }
#endif

  using InverseWaveletType = itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, WaveletFilterBankType>;
  auto inverseWavelet = InverseWaveletType::New();
  inverseWavelet->SetHighPassSubBands(highSubBands);
  inverseWavelet->SetLevels(levels);
  inverseWavelet->SetInputs(modifiedWavelets);
  inverseWavelet->SetApplyReconstructionFactors(inputApplyReconstructionFactors);
  inverseWavelet->Print(std::cout);
  inverseWavelet->Update();

  auto inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(inverseWavelet->GetOutput());
  inverseFFT->Update();

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(reader->GetOutput(), "Input Image");
  itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "Inverse Wavelet");
#endif

  // using WriterType = itk::ImageFileWriter< typename InverseFFTFilterType::OutputImageType >;
  // auto writer = WriterType::New();
  // writer->SetFileName( outputImage );
  // writer->SetInput( inverseFFT->GetOutput() );
  //
  // TRY_EXPECT_NO_EXCEPTION( writer->Update() );
  //
  return EXIT_SUCCESS;
}

int
itkStructureTensorWithGeneralizedRieszTest(int argc, char * argv[])
{
  if (argc < 8 || argc > 9)
  {
    std::cerr << "Usage: " << argv[0]
              << " inputImage outputImage inputLevels inputBands waveletFunction inputRieszOrder "
                 "applyReconstructionFactors(Apply|NoApply) [dimension]"
              << std::endl;
    return EXIT_FAILURE;
  }

  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = std::stoi(argv[3]);
  const unsigned int inputBands = std::stoi(argv[4]);
  const std::string  waveletFunction = argv[5];
  const unsigned int inputRieszOrder = std::stoi(argv[6]);
  const std::string  applyReconstructionFactorsInput = argv[7];
  bool               applyReconstructionFactors = false;
  if (applyReconstructionFactorsInput == "Apply")
  {
    applyReconstructionFactors = true;
  }
  else if (applyReconstructionFactorsInput == "NoApply")
  {
    applyReconstructionFactors = false;
  }
  else
  {
    std::cerr << "Unkown string: " + applyReconstructionFactorsInput + " . Use Apply or NoApply." << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int dimension = 3;
  if (argc == 9)
  {
    dimension = std::stoi(argv[8]);
  }

  if (dimension == 2)
  {
    using HeldWavelet = itk::HeldIsotropicWavelet<double, 2>;
    using VowWavelet = itk::VowIsotropicWavelet<double, 2>;
    using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<double, 2>;
    using ShannonWavelet = itk::ShannonIsotropicWavelet<double, 2>;
    if (waveletFunction == "Held")
    {
      return runStructureTensorWithGeneralizedRieszTest<2, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Vow")
    {
      return runStructureTensorWithGeneralizedRieszTest<2, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runStructureTensorWithGeneralizedRieszTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Shannon")
    {
      return runStructureTensorWithGeneralizedRieszTest<2, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
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
    using HeldWavelet = itk::HeldIsotropicWavelet<>;
    using VowWavelet = itk::VowIsotropicWavelet<>;
    using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<>;
    using ShannonWavelet = itk::ShannonIsotropicWavelet<>;
    if (waveletFunction == "Held")
    {
      return runStructureTensorWithGeneralizedRieszTest<3, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Vow")
    {
      return runStructureTensorWithGeneralizedRieszTest<3, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runStructureTensorWithGeneralizedRieszTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
    }
    else if (waveletFunction == "Shannon")
    {
      return runStructureTensorWithGeneralizedRieszTest<3, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, inputRieszOrder, applyReconstructionFactors);
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
