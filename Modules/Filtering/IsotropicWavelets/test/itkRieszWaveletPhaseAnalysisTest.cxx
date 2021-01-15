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
#include "itkWaveletFrequencyForwardUndecimated.h"
#include "itkWaveletFrequencyInverseUndecimated.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"

#include "itkMonogenicSignalFrequencyImageFilter.h"
#include "itkVectorInverseFFTImageFilter.h"
#include "itkPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkZeroDCImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkNumberToString.h"

#include "itkTestingMacros.h"

#include <string>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

std::string
AppendToFilenameRiesz(const std::string & filename, const std::string & appendix)
{
  std::size_t foundDot = filename.find_last_of('.');
  return filename.substr(0, foundDot) + appendix + filename.substr(foundDot);
}

// 1. Wavelet analysis (forward) on input image.
// 2. Create a Monogenic Signal (from Riesz function ) on each wavelet output..
// 3. Do a PhaseAnalysis on each Monogenic Signal.
// 4. Wavelet reconstruction (inverse) using as coefficients the output of the PhaseAnalysis.
// Without applying reconstruction factors: ApplyReconstructionFactorOff()
// 5. The result of the reconstruction will be an image that uses phase information at each level/band for improving
// local structure information, and can also work as an equalization of brightness.
template <unsigned int VDimension, typename TWaveletFunction>
int
runRieszWaveletPhaseAnalysisTest(const std::string &  inputImage,
                                 const std::string &  outputImage,
                                 const unsigned int & inputLevels,
                                 const unsigned int & inputBands,
                                 const bool           applySoftThreshold,
                                 const double         thresholdNumOfSigmas = 2.0)
{
  const unsigned int Dimension = VDimension;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  itk::NumberToString<unsigned int> n2s;
  auto                              reader = ReaderType::New();
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
  // using ForwardWaveletType = itk::WaveletFrequencyForwardUndecimated< ComplexImageType, ComplexImageType,
  // WaveletFilterBankType >;
  auto         forwardWavelet = ForwardWaveletType::New();
  unsigned int highSubBands = inputBands;
  unsigned int levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(highSubBands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftForwardFilter->GetOutput());
  forwardWavelet->Update();
  typename ForwardWaveletType::OutputsType analysisWavelets = forwardWavelet->GetOutputs();

  // Apply Monogenic signal to wavelet results
  using MonogenicSignalFrequencyFilterType = itk::MonogenicSignalFrequencyImageFilter<ComplexImageType>;
  using VectorMonoOutputType = typename MonogenicSignalFrequencyFilterType::OutputImageType;
  using VectorInverseFFTType = itk::VectorInverseFFTImageFilter<VectorMonoOutputType>;
  using PhaseAnalysisFilter =
    itk::PhaseAnalysisSoftThresholdImageFilter<typename VectorInverseFFTType::OutputImageType>;

  typename ForwardWaveletType::OutputsType modifiedWavelets;
  unsigned int                             numberOfOutputs = forwardWavelet->GetNumberOfOutputs();
  for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
  {
    std::cout << "Output #: " << i << " / " << numberOfOutputs - 1 << std::endl;
    // Held does not modify approx image.
    // If we don't do this, artifacts in the border. See #issues/72
    // TODO, the number of levels for best results change with this options:
    // In 2D, the best results are with max: if size is 512x512. M = 9, choose M
    // In 3D, the best results are following Held: 64, M = 6, choose M - 3 = 3
    if (i == numberOfOutputs - 1)
    {
      modifiedWavelets.push_back(analysisWavelets[i]);
      continue;
    }
    auto monoFilter = MonogenicSignalFrequencyFilterType::New();
    auto vecInverseFFT = VectorInverseFFTType::New();
    auto phaseAnalyzer = PhaseAnalysisFilter::New();
    auto fftForwardPhaseFilter = FFTForwardFilterType::New();

    // Generate a monogenic signal (vector valued)
    monoFilter->SetInput(analysisWavelets[i]);
    monoFilter->Update();

    vecInverseFFT->SetInput(monoFilter->GetOutput());
    vecInverseFFT->Update();

    phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());
    phaseAnalyzer->SetApplySoftThreshold(applySoftThreshold);
    if (applySoftThreshold)
    {
      phaseAnalyzer->SetNumOfSigmas(thresholdNumOfSigmas);
    }
    phaseAnalyzer->Update();

    fftForwardPhaseFilter->SetInput(phaseAnalyzer->GetOutputCosPhase());
    fftForwardPhaseFilter->Update();

    modifiedWavelets.push_back(fftForwardPhaseFilter->GetOutput());
    modifiedWavelets.back()->DisconnectPipeline();
  }

#ifdef ITK_VISUALIZE_TESTS
  // Visualize and compare modified wavelets coefficients (and approx image)
  bool visualizeCoefficients = false;
  if (visualizeCoefficients)
  {
    for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
    {
      auto inverseFFT = InverseFFTFilterType::New();
      inverseFFT->SetInput(analysisWavelets[i]);
      inverseFFT->Update();
      itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "WaveletCoef: output #" + n2s(i));
      inverseFFT->SetInput(modifiedWavelets[i]);
      inverseFFT->Update();
      itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "WaveletCoef. PhaseAnalyzed #" + n2s(i));
    }
  }
#endif

  using InverseWaveletType = itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, WaveletFilterBankType>;
  // using InverseWaveletType = itk::WaveletFrequencyInverseUndecimated< ComplexImageType, ComplexImageType,
  // WaveletFilterBankType >;
  auto inverseWavelet = InverseWaveletType::New();
  inverseWavelet->SetHighPassSubBands(highSubBands);
  inverseWavelet->SetLevels(levels);
  inverseWavelet->SetInputs(modifiedWavelets);
  // The coefficients are now phases, do not apply reconstruction factors.
  inverseWavelet->ApplyReconstructionFactorsOff();
  inverseWavelet->Update();

  auto inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(inverseWavelet->GetOutput());
  inverseFFT->Update();

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(reader->GetOutput(), "Input Image");
  itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "Inverse Wavelet");
#endif

  // Cast To Float for save as tiff.
  using ImageFloatType = itk::Image<float, Dimension>;
  using CastFloatType = itk::CastImageFilter<ImageType, ImageFloatType>;
  auto caster = CastFloatType::New();
  caster->SetInput(inverseFFT->GetOutput());
  caster->Update();

  // using WriterType = itk::ImageFileWriter< typename InverseFFTFilterType::OutputImageType >;
  using WriterType = itk::ImageFileWriter<ImageFloatType>;
  auto        writer = WriterType::New();
  std::string appendString = "_L" + n2s(inputLevels) + "_B" + n2s(inputBands) + "_S" + n2s(thresholdNumOfSigmas);
  std::string outputFile = AppendToFilenameRiesz(outputImage, appendString);
  writer->SetFileName(outputFile);
  writer->SetInput(caster->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  //
  return EXIT_SUCCESS;
}

int
itkRieszWaveletPhaseAnalysisTest(int argc, char * argv[])
{
  if (argc < 8 || argc > 9)
  {
    std::cerr
      << "Usage: " << argv[0]
      << " inputImage outputImage inputLevels inputBands waveletFunction dimension Apply|NoApply [thresholdNumOfSigmas]"
      << std::endl;
    return EXIT_FAILURE;
  }

  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = std::stoi(argv[3]);
  const unsigned int inputBands = std::stoi(argv[4]);
  const std::string  waveletFunction = argv[5];
  const unsigned int dimension = std::stoi(argv[6]);
  const std::string  applySoftThresholdInput = argv[7];
  bool               applySoftThreshold = false;
  if (applySoftThresholdInput == "Apply")
  {
    applySoftThreshold = true;
  }
  else if (applySoftThresholdInput == "NoApply")
  {
    applySoftThreshold = false;
  }
  else
  {
    std::cerr << "Unknown string: " + applySoftThresholdInput + " . Use Apply or NoApply." << std::endl;
    return EXIT_FAILURE;
  }

  double thresholdNumOfSigmas = 2.0;
  if (argc == 9)
  {
    thresholdNumOfSigmas = std::stod(argv[8]);
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

  using HeldInverseWaveletType =
    itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, HeldWaveletFilterBankType>;
  auto heldInverseWavelet = HeldInverseWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(heldInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  using VowForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>;
  auto vowForwardWavelet = VowForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(vowForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using VowInverseWaveletType =
    itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>;
  auto vowInverseWavelet = VowInverseWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(vowInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  using SimoncelliForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>;
  auto simoncelliForwardWavelet = SimoncelliForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(simoncelliForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using SimoncelliInverseWaveletType =
    itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>;
  auto simoncelliInverseWavelet = SimoncelliInverseWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(simoncelliInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  using ShannonForwardWaveletType =
    itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>;
  auto shannonForwardWavelet = ShannonForwardWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(shannonForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  using ShannonInverseWaveletType =
    itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>;
  auto shannonInverseWavelet = ShannonInverseWaveletType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(shannonInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runRieszWaveletPhaseAnalysisTest<2, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Vow")
    {
      return runRieszWaveletPhaseAnalysisTest<2, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runRieszWaveletPhaseAnalysisTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Shannon")
    {
      return runRieszWaveletPhaseAnalysisTest<2, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
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
      return runRieszWaveletPhaseAnalysisTest<3, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Vow")
    {
      return runRieszWaveletPhaseAnalysisTest<3, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runRieszWaveletPhaseAnalysisTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
    else if (waveletFunction == "Shannon")
    {
      return runRieszWaveletPhaseAnalysisTest<3, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
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
