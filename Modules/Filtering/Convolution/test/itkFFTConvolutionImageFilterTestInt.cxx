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

#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

#include "itkObjectFactoryBase.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

int
itkFFTConvolutionImageFilterTestInt(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage kernelImage outputImage [normalizeImage] [outputRegionMode]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int ImageDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());

  using ConvolutionFilterType = itk::FFTConvolutionImageFilter<ImageType>;
  auto convolver = ConvolutionFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(convolver, FFTConvolutionImageFilter, ConvolutionImageFilterBase);

  convolver->SetInput(reader1->GetOutput());
  convolver->SetKernelImage(reader2->GetOutput());

  constexpr ConvolutionFilterType::SizeValueType sizeGreatestPrimeFactor = 2;
  convolver->SetSizeGreatestPrimeFactor(sizeGreatestPrimeFactor);
  ITK_TEST_SET_GET_VALUE(sizeGreatestPrimeFactor, convolver->GetSizeGreatestPrimeFactor());

  const itk::SimpleFilterWatcher watcher(convolver, "filter");

  if (argc >= 5)
  {
    convolver->SetNormalize(static_cast<bool>(std::stoi(argv[4])));
  }

  if (argc >= 6)
  {
    const std::string outputRegionMode(argv[5]);
    if (outputRegionMode == "SAME")
    {
      convolver->SetOutputRegionModeToSame();
      ITK_TEST_SET_GET_VALUE(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME,
                             convolver->GetOutputRegionMode());
      std::cout << "OutputRegionMode set to SAME." << std::endl;
    }
    else if (outputRegionMode == "VALID")
    {
      convolver->SetOutputRegionModeToValid();
      ITK_TEST_SET_GET_VALUE(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID,
                             convolver->GetOutputRegionMode());
      std::cout << "OutputRegionMode set to VALID." << std::endl;
    }
    else
    {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << std::endl;
      std::cerr << "Valid values are SAME or VALID." << std::endl;
      return EXIT_FAILURE;
    }
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(convolver->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(convolver->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
