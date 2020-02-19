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

#include "itkWaveletCoeffsPhaseAnalyzisImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkNumberToString.h"

#include <string>

std::string
AppendToOutputFilename(const std::string & filename, const std::string & appendix)
{
  std::size_t foundDot = filename.find_last_of('.');
  return filename.substr(0, foundDot) + appendix + filename.substr(foundDot);
}

template <unsigned int VDimension, typename TWavelet>
int
runWaveletCoeffsPhaseAnalyzisImageFilterTest(const std::string &  inputImage,
                                             const std::string &  outputImage,
                                             const unsigned int & inputLevels,
                                             const unsigned int & inputBands,
                                             const bool           applySoftThreshold,
                                             const double         thresholdNumOfSigmas = 2.0)
{
  const unsigned int Dimension = VDimension;

  using ImageType = itk::Image<float, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);

  // Perform phase analysis on wavelet coefficients of the input image.
  using WaveletCoeffsPhaseAnalyzisImageFilterType = itk::WaveletCoeffsPhaseAnalyzisImageFilter<ImageType, TWavelet>;
  auto waveletCoeffsPhaseAnalyzisImageFilter = WaveletCoeffsPhaseAnalyzisImageFilterType::New();
  waveletCoeffsPhaseAnalyzisImageFilter->SetInput(reader->GetOutput());

  waveletCoeffsPhaseAnalyzisImageFilter->SetLevels(inputLevels);
  waveletCoeffsPhaseAnalyzisImageFilter->SetHighPassSubBands(inputBands);

  waveletCoeffsPhaseAnalyzisImageFilter->SetApplySoftThreshold(applySoftThreshold);
  waveletCoeffsPhaseAnalyzisImageFilter->SetThresholdNumOfSigmas(thresholdNumOfSigmas);

  TRY_EXPECT_NO_EXCEPTION(waveletCoeffsPhaseAnalyzisImageFilter->Update());

  // Write output image
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(waveletCoeffsPhaseAnalyzisImageFilter->GetOutput());

  itk::NumberToString<unsigned int> n2s;
  std::string appendString = "_L" + n2s(inputLevels) + "_B" + n2s(inputBands) + "_S" + n2s(thresholdNumOfSigmas);
  std::string outputFile = AppendToOutputFilename(outputImage, appendString);

  writer->SetFileName(outputFile);
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}

int
itkWaveletCoeffsPhaseAnalyzisImageFilterTest(int argc, char * argv[])
{
  if (argc < 8 || argc > 9)
  {
    std::cerr << "Usage : " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile inputLevels inputBands waveletFunction dimension Apply|NoApply "
                 "[thresholdNumOfSigmas]"
              << std::endl;
    return EXIT_FAILURE;
  }
  const std::string      inputImage = argv[1];
  const std::string      outputImage = argv[2];
  const unsigned int     inputLevels = atoi(argv[3]);
  const unsigned int     inputBands = atoi(argv[4]);
  const unsigned int     dimension = atoi(argv[6]);
  constexpr unsigned int ImageDimension = 3;
  if (dimension != ImageDimension)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
  const std::string applySoftThresholdInput = argv[7];
  bool              applySoftThreshold = false;
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
  double thresholdNumOfSigmas = 2.0;
  if (argc == 9)
  {
    thresholdNumOfSigmas = atof(argv[8]);
  }

  using ImageType = itk::Image<float, ImageDimension>;

  using WaveletScalarType = double;
  const std::string waveletFunction = argv[5];
  if (waveletFunction == "Held")
  {
    using WaveletType = itk::HeldIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsPhaseAnalyzisImageFilterType =
        itk::WaveletCoeffsPhaseAnalyzisImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsPhaseAnalyzisImageFilter = WaveletCoeffsPhaseAnalyzisImageFilterType::New();
      EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsPhaseAnalyzisImageFilter, WaveletCoeffsPhaseAnalyzisImageFilter, ImageToImageFilter);

      return runWaveletCoeffsPhaseAnalyzisImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
  }
  else if (waveletFunction == "Vow")
  {
    using WaveletType = itk::VowIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsPhaseAnalyzisImageFilterType =
        itk::WaveletCoeffsPhaseAnalyzisImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsPhaseAnalyzisImageFilter = WaveletCoeffsPhaseAnalyzisImageFilterType::New();
      EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsPhaseAnalyzisImageFilter, WaveletCoeffsPhaseAnalyzisImageFilter, ImageToImageFilter);

      return runWaveletCoeffsPhaseAnalyzisImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
  }
  else if (waveletFunction == "Simoncelli")
  {
    using WaveletType = itk::SimoncelliIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsPhaseAnalyzisImageFilterType =
        itk::WaveletCoeffsPhaseAnalyzisImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsPhaseAnalyzisImageFilter = WaveletCoeffsPhaseAnalyzisImageFilterType::New();
      EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsPhaseAnalyzisImageFilter, WaveletCoeffsPhaseAnalyzisImageFilter, ImageToImageFilter);

      return runWaveletCoeffsPhaseAnalyzisImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
  }
  else if (waveletFunction == "Shannon")
  {
    using WaveletType = itk::ShannonIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsPhaseAnalyzisImageFilterType =
        itk::WaveletCoeffsPhaseAnalyzisImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsPhaseAnalyzisImageFilter = WaveletCoeffsPhaseAnalyzisImageFilterType::New();
      EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsPhaseAnalyzisImageFilter, WaveletCoeffsPhaseAnalyzisImageFilter, ImageToImageFilter);

      return runWaveletCoeffsPhaseAnalyzisImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands, applySoftThreshold, thresholdNumOfSigmas);
    }
  }
  std::cerr << " failed!" << std::endl;
  std::cerr << waveletFunction << " wavelet type not supported." << std::endl;
  return EXIT_FAILURE;
}
