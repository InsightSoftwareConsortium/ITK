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

#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyInverse.h"
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

#include "itkTestingMacros.h"

#include <string>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#  include "itkNumberToString.h"
#endif

// 1. Wavelet analysis (forward) on input image.
// 2. Create a Monogenic Signal (from Riesz function ) on each wavelet output..
// 3. Do a PhaseAnalysis on each Monogenic Signal.
// 4. Wavelet reconstruction (inverse) using as coefficients the output of the PhaseAnalysis.
// Without applying reconstruction factors: ApplyReconstructionFactorOff()
// 5. The result of the reconstruction will be an image that uses phase information at each level/band for improving
// local structure information, and can also work as an equalizator of brightness.
template <unsigned int VDimension, typename TWaveletFunction>
int
runRieszWaveletPhaseAnalysisTest(const std::string & inputImage,
                                 const std::string &, // outputImage
                                 const unsigned int & inputLevels,
                                 const unsigned int & inputBands,
                                 const bool           applySoftThreshold)
{
  const unsigned int Dimension = VDimension;

  typedef double                           PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  zeroDCFilter->Update();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<typename ZeroDCFilterType::OutputImageType> FFTForwardFilterType;
  typename FFTForwardFilterType::Pointer fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(zeroDCFilter->GetOutput());
  fftForwardFilter->Update();
  typedef typename FFTForwardFilterType::OutputImageType ComplexImageType;

  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;

  // Forward Wavelet
  typedef TWaveletFunction                                                                        WaveletFunctionType;
  typedef itk::WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>         WaveletFilterBankType;
  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType> ForwardWaveletType;
  typename ForwardWaveletType::Pointer forwardWavelet = ForwardWaveletType::New();
  unsigned int                         highSubBands = inputBands;
  unsigned int                         levels = inputLevels;
  forwardWavelet->SetHighPassSubBands(highSubBands);
  forwardWavelet->SetLevels(levels);
  forwardWavelet->SetInput(fftForwardFilter->GetOutput());
  forwardWavelet->Update();
  typename ForwardWaveletType::OutputsType analysisWavelets = forwardWavelet->GetOutputs();

  // Apply Monogenic signal to wavelet results
  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType>   MonogenicSignalFrequencyFilterType;
  typedef typename MonogenicSignalFrequencyFilterType::OutputImageType VectorMonoOutputType;
  typedef itk::VectorInverseFFTImageFilter<VectorMonoOutputType>       VectorInverseFFTType;
  typedef itk::PhaseAnalysisSoftThresholdImageFilter<typename VectorInverseFFTType::OutputImageType>
    PhaseAnalysisFilter;

  typename ForwardWaveletType::OutputsType modifiedWavelets;
  unsigned int                             numberOfOutputs = forwardWavelet->GetNumberOfOutputs();
  for (unsigned int i = 0; i < forwardWavelet->GetNumberOfOutputs(); ++i)
  {
    std::cout << "Output #: " << i << " / " << numberOfOutputs - 1 << std::endl;
    typename MonogenicSignalFrequencyFilterType::Pointer monoFilter = MonogenicSignalFrequencyFilterType::New();
    typename VectorInverseFFTType::Pointer               vecInverseFFT = VectorInverseFFTType::New();
    typename PhaseAnalysisFilter::Pointer                phaseAnalyzer = PhaseAnalysisFilter::New();
    typename FFTForwardFilterType::Pointer               fftForwardPhaseFilter = FFTForwardFilterType::New();

    // Generate a monogenic signal (vector valued)
    monoFilter->SetInput(analysisWavelets[i]);
    monoFilter->Update();

    vecInverseFFT->SetInput(monoFilter->GetOutput());
    vecInverseFFT->Update();

    phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());
    phaseAnalyzer->SetApplySoftThreshold(applySoftThreshold);
    // if( i == numberOfOutputs - 1 ) // TODO Held does not modify approx image, but it does not generate better
    // results.
    //   {
    //   phaseAnalyzer->SetApplySoftThreshold( false );
    //   }
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
      itk::NumberToString<unsigned int>      n2s;
      typename InverseFFTFilterType::Pointer inverseFFT = InverseFFTFilterType::New();
      inverseFFT->SetInput(analysisWavelets[i]);
      inverseFFT->Update();
      itk::Testing::ViewImage(inverseFFT->GetOutput(), "WaveletCoef: output #" + n2s(i));
      inverseFFT->SetInput(modifiedWavelets[i]);
      inverseFFT->Update();
      itk::Testing::ViewImage(inverseFFT->GetOutput(), "WaveletCoef. PhaseAnalyzed #" + n2s(i));
    }
  }
#endif

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, WaveletFilterBankType> InverseWaveletType;
  typename InverseWaveletType::Pointer inverseWavelet = InverseWaveletType::New();
  inverseWavelet->SetHighPassSubBands(highSubBands);
  inverseWavelet->SetLevels(levels);
  inverseWavelet->SetInputs(modifiedWavelets);
  // The coefficients are now phases, do not apply reconstrucction factors.
  inverseWavelet->ApplyReconstructionFactorsOff();
  inverseWavelet->Update();

  typename InverseFFTFilterType::Pointer inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(inverseWavelet->GetOutput());
  inverseFFT->Update();

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(reader->GetOutput(), "Input Image");
  itk::Testing::ViewImage(inverseFFT->GetOutput(), "Inverse Wavelet");
#endif

  // typedef itk::ImageFileWriter< typename InverseFFTFilterType::OutputImageType > WriterType;
  // typename WriterType::Pointer writer = WriterType::New();
  // writer->SetFileName( outputImage );
  // writer->SetInput( inverseFFT->GetOutput() );
  //
  // TRY_EXPECT_NO_EXCEPTION( writer->Update() );
  //
  return EXIT_SUCCESS;
}

int
itkRieszWaveletPhaseAnalysisTest(int argc, char * argv[])
{
  if (argc < 7 || argc > 8)
  {
    std::cerr << "Usage: " << argv[0]
              << " inputImage outputImage inputLevels inputBands waveletFunction Apply|NoApply [dimension]"
              << std::endl;
    return EXIT_FAILURE;
  }

  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = atoi(argv[3]);
  const unsigned int inputBands = atoi(argv[4]);
  const std::string  waveletFunction = argv[5];
  const std::string  applySoftThresholdInput = argv[6];
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
    std::cerr << "Unkown string: " + applySoftThresholdInput + " . Use Apply or NoApply." << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int dimension = 3;
  if (argc == 8)
  {
    dimension = atoi(argv[7]);
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

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, HeldWaveletFilterBankType>
                                  HeldForwardWaveletType;
  HeldForwardWaveletType::Pointer heldForwardWavelet = HeldForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(heldForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, HeldWaveletFilterBankType>
                                  HeldInverseWaveletType;
  HeldInverseWaveletType::Pointer heldInverseWavelet = HeldInverseWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(heldInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>
                                 VowForwardWaveletType;
  VowForwardWaveletType::Pointer vowForwardWavelet = VowForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(vowForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, VowWaveletFilterBankType>
                                 VowInverseWaveletType;
  VowInverseWaveletType::Pointer vowInverseWavelet = VowInverseWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(vowInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>
                                        SimoncelliForwardWaveletType;
  SimoncelliForwardWaveletType::Pointer simoncelliForwardWavelet = SimoncelliForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(simoncelliForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, SimoncelliWaveletFilterBankType>
                                        SimoncelliInverseWaveletType;
  SimoncelliInverseWaveletType::Pointer simoncelliInverseWavelet = SimoncelliInverseWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(simoncelliInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  typedef itk::WaveletFrequencyForward<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>
                                     ShannonForwardWaveletType;
  ShannonForwardWaveletType::Pointer shannonForwardWavelet = ShannonForwardWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(shannonForwardWavelet, WaveletFrequencyForward, ImageToImageFilter);

  typedef itk::WaveletFrequencyInverse<ComplexImageType, ComplexImageType, ShannonWaveletFilterBankType>
                                     ShannonInverseWaveletType;
  ShannonInverseWaveletType::Pointer shannonInverseWavelet = ShannonInverseWaveletType::New();
  EXERCISE_BASIC_OBJECT_METHODS(shannonInverseWavelet, WaveletFrequencyInverse, ImageToImageFilter);

  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runRieszWaveletPhaseAnalysisTest<2, HeldWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Vow")
    {
      return runRieszWaveletPhaseAnalysisTest<2, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runRieszWaveletPhaseAnalysisTest<2, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Shannon")
    {
      return runRieszWaveletPhaseAnalysisTest<2, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
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
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Vow")
    {
      return runRieszWaveletPhaseAnalysisTest<3, VowWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runRieszWaveletPhaseAnalysisTest<3, SimoncelliWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
    }
    else if (waveletFunction == "Shannon")
    {
      return runRieszWaveletPhaseAnalysisTest<3, ShannonWavelet>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold);
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
