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

  const unsigned int                       Dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(inputImage);

  TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTForwardFilterType;
  FFTForwardFilterType::Pointer                 fftForwardFilter = FFTForwardFilterType::New();

  fftForwardFilter->SetInput(reader->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(fftForwardFilter->Update());

  typedef FFTForwardFilterType::OutputImageType ComplexImageType;

  // Get a Monogenic Vector. Other input to PhaseAnalysis could be derivatives.
  typedef itk::MonogenicSignalFrequencyImageFilter<ComplexImageType> MonogenicSignalFrequencyFilterType;
  MonogenicSignalFrequencyFilterType::Pointer monoFilter = MonogenicSignalFrequencyFilterType::New();

  monoFilter->SetInput(fftForwardFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(monoFilter->Update());

  typedef MonogenicSignalFrequencyFilterType::OutputImageType VectorMonoOutputType;

  typedef itk::VectorInverseFFTImageFilter<VectorMonoOutputType> VectorInverseFFTType;
  VectorInverseFFTType::Pointer                                  vecInverseFFT = VectorInverseFFTType::New();

  vecInverseFFT->SetInput(monoFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(vecInverseFFT->Update());

  // Input to the PhaseAnalysisSoftThreshold
  typedef itk::PhaseAnalysisSoftThresholdImageFilter<VectorInverseFFTType::OutputImageType>
                                                PhaseAnalysisSoftThresholdFilterType;
  PhaseAnalysisSoftThresholdFilterType::Pointer phaseAnalyzer = PhaseAnalysisSoftThresholdFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(phaseAnalyzer, PhaseAnalysisSoftThresholdImageFilter, PhaseAnalysisImageFilter);

  bool applySoftThreshold = static_cast<bool>(atoi(argv[3]));
  TEST_SET_GET_BOOLEAN(phaseAnalyzer, ApplySoftThreshold, applySoftThreshold);

  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType numOfSigmas =
    static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(atof(argv[4]));
  phaseAnalyzer->SetNumOfSigmas(numOfSigmas);
  TEST_SET_GET_VALUE(numOfSigmas, phaseAnalyzer->GetNumOfSigmas());

  phaseAnalyzer->SetInput(vecInverseFFT->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(phaseAnalyzer->Update());


  // Regression tests
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType expectedMeanAmp =
    static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(atof(argv[5]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedMeanAmp = phaseAnalyzer->GetMeanAmp();
  if (itk::Math::NotAlmostEquals(expectedMeanAmp, computedMeanAmp))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetMeanAmp()" << std::endl;
    std::cerr << "Expected: " << expectedMeanAmp << ", but got: " << computedMeanAmp << std::endl;
    testStatus = EXIT_FAILURE;
  }

  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType expectedSigmaAmp =
    static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(atof(argv[6]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedSigmaAmp = phaseAnalyzer->GetSigmaAmp();
  if (itk::Math::NotAlmostEquals(expectedSigmaAmp, computedSigmaAmp))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetSigmaAmp()" << std::endl;
    std::cerr << "Expected: " << expectedSigmaAmp << ", but got: " << computedSigmaAmp << std::endl;
    testStatus = EXIT_FAILURE;
  }

  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType expectedThreshold =
    static_cast<PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType>(atof(argv[7]));
  PhaseAnalysisSoftThresholdFilterType::OutputImagePixelType computedThreshold = phaseAnalyzer->GetThreshold();
  if (itk::Math::NotAlmostEquals(expectedThreshold, computedThreshold))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetThreshold()" << std::endl;
    std::cerr << "Expected: " << expectedThreshold << ", but got: " << computedThreshold << std::endl;
    testStatus = EXIT_FAILURE;
  }

  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer cosPhase = phaseAnalyzer->GetOutputCosPhase();
  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer amp = phaseAnalyzer->GetOutputAmplitude();
  PhaseAnalysisSoftThresholdFilterType::OutputImageType::Pointer phase = phaseAnalyzer->GetOutputPhase();

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(cosPhase.GetPointer(), "PhaseAnalyzer(Soft) output");
#endif

  return testStatus;
}
