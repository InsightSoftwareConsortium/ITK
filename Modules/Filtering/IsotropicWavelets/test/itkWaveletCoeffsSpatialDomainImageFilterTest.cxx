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

#include "itkWaveletCoeffsSpatialDomainImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkNumberToString.h"

#include <string>

std::string
AppendToDifferentOutputFilename(const std::string & filename, const std::string & appendix)
{
  std::size_t foundDot = filename.find_last_of('.');
  return filename.substr(0, foundDot) + appendix + filename.substr(foundDot);
}

template <unsigned int VDimension, typename TWavelet>
int
runWaveletCoeffsSpatialDomainImageFilterTest(const std::string &  inputImage,
                                             const std::string &  outputImage,
                                             const unsigned int & inputLevels,
                                             const unsigned int & inputBands)
{
  const unsigned int Dimension = VDimension;

  using ImageType = itk::Image<float, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);

  // Perform wavelet coefficients extraction on input image.
  using WaveletCoeffsSpatialDomainImageFilterType = itk::WaveletCoeffsSpatialDomainImageFilter<ImageType, TWavelet>;
  auto waveletCoeffsSpatialDomainImageFilter = WaveletCoeffsSpatialDomainImageFilterType::New();
  waveletCoeffsSpatialDomainImageFilter->SetInput(reader->GetOutput());

  waveletCoeffsSpatialDomainImageFilter->SetLevels(inputLevels);
  waveletCoeffsSpatialDomainImageFilter->SetHighPassSubBands(inputBands);

  ITK_TRY_EXPECT_NO_EXCEPTION(waveletCoeffsSpatialDomainImageFilter->Update());

  itk::NumberToString<unsigned int> n2s;
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();

  waveletCoeffsSpatialDomainImageFilter->Update();

  unsigned int TotalOutputs = inputLevels * inputBands + 1;
  for (unsigned int i = 0; i < TotalOutputs; ++i)
  {
    writer->SetInput(waveletCoeffsSpatialDomainImageFilter->GetOutput(i));

    std::string appendString = "_L" + n2s(inputLevels) + "_B" + n2s(inputBands) + "_i" + n2s(i);
    std::string outputFile = AppendToDifferentOutputFilename(outputImage, appendString);
    writer->SetFileName(outputFile);
    writer->UseCompressionOn();

    try
    {
      ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
    }
    catch (itk::ExceptionObject & e)
    {
      std::cerr << "Error : " << e << std::endl;
    }
    writer->ResetPipeline();
  }

  return EXIT_SUCCESS;
}

int
itkWaveletCoeffsSpatialDomainImageFilterTest(int argc, char * argv[])
{
  if (argc != 7)
  {
    std::cerr << "Usage : " << std::endl;
    std::cerr << argv[0] << " inputImageFile outputImageFile inputLevels inputBands waveletFunction dimension"
              << std::endl;
    return EXIT_FAILURE;
  }
  const std::string  inputImage = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputLevels = atoi(argv[3]);
  const unsigned int inputBands = atoi(argv[4]);
  const unsigned int dimension = atoi(argv[6]);
  constexpr size_t   ImageDimension = 3;
  if (!(dimension == ImageDimension))
  {
    std::cerr << "Only 3 dimension supported." << std::endl;
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
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
      using WaveletCoeffsSpatialDomainImageFilterType =
        itk::WaveletCoeffsSpatialDomainImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsSpatialDomainImageFilter = WaveletCoeffsSpatialDomainImageFilterType::New();
      ITK_EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsSpatialDomainImageFilter, WaveletCoeffsSpatialDomainImageFilter, ImageToImageFilter);

      return runWaveletCoeffsSpatialDomainImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands);
    }
  }
  else if (waveletFunction == "Vow")
  {
    using WaveletType = itk::VowIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsSpatialDomainImageFilterType =
        itk::WaveletCoeffsSpatialDomainImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsSpatialDomainImageFilter = WaveletCoeffsSpatialDomainImageFilterType::New();
      ITK_EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsSpatialDomainImageFilter, WaveletCoeffsSpatialDomainImageFilter, ImageToImageFilter);

      return runWaveletCoeffsSpatialDomainImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands);
    }
  }
  else if (waveletFunction == "Simoncelli")
  {
    using WaveletType = itk::SimoncelliIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsSpatialDomainImageFilterType =
        itk::WaveletCoeffsSpatialDomainImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsSpatialDomainImageFilter = WaveletCoeffsSpatialDomainImageFilterType::New();
      ITK_EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsSpatialDomainImageFilter, WaveletCoeffsSpatialDomainImageFilter, ImageToImageFilter);

      return runWaveletCoeffsSpatialDomainImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands);
    }
  }
  else if (waveletFunction == "Shannon")
  {
    using WaveletType = itk::ShannonIsotropicWavelet<WaveletScalarType, ImageDimension>;
    {
      // Exercise basic object methods
      // Done outside the helper function in the test because GCC is limited
      // when calling overloaded base class functions.
      using WaveletCoeffsSpatialDomainImageFilterType =
        itk::WaveletCoeffsSpatialDomainImageFilter<ImageType, WaveletType>;

      auto waveletCoeffsSpatialDomainImageFilter = WaveletCoeffsSpatialDomainImageFilterType::New();
      ITK_EXERCISE_BASIC_OBJECT_METHODS(
        waveletCoeffsSpatialDomainImageFilter, WaveletCoeffsSpatialDomainImageFilter, ImageToImageFilter);

      return runWaveletCoeffsSpatialDomainImageFilterTest<3, WaveletType>(
        inputImage, outputImage, inputLevels, inputBands);
    }
  }
  std::cerr << " failed!" << std::endl;
  std::cerr << waveletFunction << " wavelet type not supported." << std::endl;
  return EXIT_FAILURE;
}
