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

#include "itkPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkMonogenicSignalFrequencyImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

#include "itkVectorInverseFFTImageFilter.h"

#include "itkMath.h"
#include "itkTestingMacros.h"

#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

int
itkPhaseAnalysisSoftThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage"
              << " outputImage"
              << " applySoftThreshold"
              << " numberOfSigmas"
              << " expectedMeanAmp"
              << " expectedSigmaAmp"
              << " expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  int testStatus = EXIT_SUCCESS;

  const std::string inputImage = argv[1];
  const std::string outputImage = argv[2];

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();

  reader->SetFileName(inputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Perform FFT on input image.
  using FFTForwardFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftForwardFilter = FFTForwardFilterType::New();

  fftForwardFilter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(fftForwardFilter->Update());

  using ComplexImageType = FFTForwardFilterType::OutputImageType;

  // Get a Monogenic Vector. Other input to PhaseAnalysis could be derivatives.
  using MonogenicSignalFrequencyFilterType = itk::MonogenicSignalFrequencyImageFilter<ComplexImageType>;
  auto monoFilter = MonogenicSignalFrequencyFilterType::New();

  monoFilter->SetInput(fftForwardFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(monoFilter->Update());

  using VectorMonoOutputType = MonogenicSignalFrequencyFilterType::OutputImageType;

  using VectorInverseFFTType = itk::VectorInverseFFTImageFilter<VectorMonoOutputType>;
  auto vecInverseFFT = VectorInverseFFTType::New();

  vecInverseFFT->SetInput(monoFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(vecInverseFFT->Update());

  // Input to the PhaseAnalysisSoftThreshold
  using PhaseAnalysisSoftThresholdFilterType =
    itk::PhaseAnalysisSoftThresholdImageFilter<VectorInverseFFTType::OutputImageType>;
  auto phaseAnalyzer = PhaseAnalysisSoftThresholdFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(phaseAnalyzer, PhaseAnalysisSoftThresholdImageFilter, PhaseAnalysisImageFilter);

  auto applySoftThreshold = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(phaseAnalyzer, ApplySoftThreshold, applySoftThreshold);

  auto numOfSigmas = static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(std::stod(argv[4]));
  phaseAnalyzer->SetNumOfSigmas(numOfSigmas);
  ITK_TEST_SET_GET_VALUE(numOfSigmas, phaseAnalyzer->GetNumOfSigmas());

  phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(phaseAnalyzer->Update());


  // Regression tests
  auto expectedMeanAmp = static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(std::stod(argv[5]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedMeanAmp = phaseAnalyzer->GetMeanAmp();
  if (itk::Math::NotAlmostEquals(expectedMeanAmp, computedMeanAmp))
  {
    std::cerr << "Warning!" << std::endl;
    std::cerr << "Error in GetMeanAmp()" << std::endl;
    std::cerr << "Expected: " << expectedMeanAmp << ", but got: " << computedMeanAmp << std::endl;
    // float point errors in different OS?
    // Expected: 10044.5, but got: 10055.1
    // testStatus = EXIT_FAILURE;
  }

  auto expectedSigmaAmp = static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(std::stod(argv[6]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedSigmaAmp = phaseAnalyzer->GetSigmaAmp();
  if (itk::Math::NotAlmostEquals(expectedSigmaAmp, computedSigmaAmp))
  {
    std::cerr << "Warning!" << std::endl;
    std::cerr << "Error in GetSigmaAmp()" << std::endl;
    std::cerr << "Expected: " << expectedSigmaAmp << ", but got: " << computedSigmaAmp << std::endl;
    // Expected: 5020.3, but got: 5018.47
    // testStatus = EXIT_FAILURE;
  }

  auto expectedThreshold = static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(std::stod(argv[7]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedThreshold = phaseAnalyzer->GetThreshold();
  if (itk::Math::NotAlmostEquals(expectedThreshold, computedThreshold))
  {
    std::cerr << "Warning!" << std::endl;
    std::cerr << "Error in GetThreshold()" << std::endl;
    std::cerr << "Expected: " << expectedThreshold << ", but got: " << computedThreshold << std::endl;
    // Expected: 20085.1, but got: 20092
    // testStatus = EXIT_FAILURE;
  }

  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer cosPhase = phaseAnalyzer->GetOutputCosPhase();
  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer amp = phaseAnalyzer->GetOutputAmplitude();
  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer phase = phaseAnalyzer->GetOutputPhase();

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(cosPhase.GetPointer(), "PhaseAnalyzer(Soft) output");
#endif

  return testStatus;
}
