/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkStdStreamLogOutput.h"
#include "itkPatchBasedDenoisingImageFilter.h"
#include "itkTestingMacros.h"


template <typename ImageT>
int
doDenoising(const std::string & inputFileName, const std::string & outputFileName)
{
  using ReaderType = itk::ImageFileReader<ImageT>;

  using FilterType = itk::PatchBasedDenoisingImageFilter<ImageT, ImageT>;

  using OutputImageType = typename FilterType::OutputImageType;

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  // Read the noisy image to be denoised
  auto reader = ReaderType::New();
  reader->SetFileName(inputFileName);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Create filter and initialize
  auto filter = FilterType::New();

  ITK_TEST_SET_GET_BOOLEAN(filter, UseSmoothDiscPatchWeights, true);
  auto kernelBandwidthSigma = typename FilterType::RealArrayType{};
  ITK_TEST_SET_GET_VALUE(kernelBandwidthSigma, filter->GetKernelBandwidthSigma());
  ITK_TEST_SET_GET_VALUE(0.20, filter->GetKernelBandwidthFractionPixelsForEstimation());
  ITK_TEST_SET_GET_BOOLEAN(filter, ComputeConditionalDerivatives, false);
  ITK_TEST_SET_GET_BOOLEAN(filter, UseFastTensorComputations, true);
  ITK_TEST_SET_GET_VALUE(1.0, filter->GetKernelBandwidthMultiplicationFactor());
  ITK_TEST_SET_GET_VALUE(0, filter->GetNoiseSigma());

  // Use 2 threads for consistency
  filter->SetNumberOfWorkUnits(2);

  filter->SetInput(reader->GetOutput());

  // Denoise the image
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  std::cout << "NumIndependentComponents: " << filter->GetNumIndependentComponents() << std::endl;

  // Write the denoised image to file
  auto writer = WriterType::New();
  writer->SetFileName(outputFileName);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}

int
itkPatchBasedDenoisingImageFilterDefaultTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFileName outputImageFileName"
              << " numDimensions" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using PixelType = float;
  using ImageType = itk::Image<PixelType, 3>;
  using FilterType = itk::PatchBasedDenoisingImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, PatchBasedDenoisingImageFilter, PatchBasedDenoisingBaseImageFilter);


  const std::string inFileName(argv[1]);

  const std::string outFileName(argv[2]);

  const unsigned int numDimensions = std::stoi(argv[3]);

  using PixelComponentType = float;

  using OneComponentType = PixelComponentType;

  using OneComponent2DImage = itk::Image<OneComponentType, 2>;
  using OneComponent3DImage = itk::Image<OneComponentType, 3>;

  if (numDimensions == 2)
  {
    return doDenoising<OneComponent2DImage>(inFileName, outFileName);
  }
  else if (numDimensions == 3)
  {
    return doDenoising<OneComponent3DImage>(inFileName, outFileName);
  }
  else
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << numDimensions << " dimensions "
              << "isn't supported in this test driver." << std::endl;
    return EXIT_FAILURE;
  }
}
