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

#include "itkChangeInformationImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPeriodicBoundaryCondition.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

int
itkFFTConvolutionImageFilterTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage "
              << "kernelImage "
              << "outputImage "
              << "[sizeGreatestPrimeFactor] "
              << "[normalizeImage] "
              << "[outputRegionMode] "
              << "[boundaryCondition] " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());

  using ConvolutionFilterType = itk::FFTConvolutionImageFilter<ImageType>;
  ConvolutionFilterType::Pointer convoluter = ConvolutionFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(convoluter, FFTConvolutionImageFilter, ConvolutionImageFilterBase);

  // Test empty image exception
  ImageType::Pointer emptyImage = ImageType::New();
  convoluter->SetInput(emptyImage);
  try
  {
    convoluter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << excp << std::endl;
    std::cout << "Caught EXPECTED exception for empty image as input" << std::endl;
  }

  // Test generality of filter by changing the image index
  using ChangeInformationFilterType = itk::ChangeInformationImageFilter<ImageType>;
  ChangeInformationFilterType::Pointer inputChanger = ChangeInformationFilterType::New();
  inputChanger->ChangeRegionOn();
  ImageType::OffsetType inputOffset = { { -2, 3 } };
  inputChanger->SetOutputOffset(inputOffset);
  inputChanger->SetInput(reader1->GetOutput());

  convoluter->SetInput(inputChanger->GetOutput());

  // Test generality of filter by changing the kernel index
  ChangeInformationFilterType::Pointer kernelChanger = ChangeInformationFilterType::New();
  kernelChanger->ChangeRegionOn();
  ImageType::OffsetType kernelOffset = { { 3, -5 } };
  kernelChanger->SetOutputOffset(kernelOffset);
  kernelChanger->SetInput(reader2->GetOutput());

  convoluter->SetKernelImage(kernelChanger->GetOutput());

  if (argc >= 5)
  {
    ConvolutionFilterType::SizeValueType sizeGreatestPrimeFactor = std::stoi(argv[4]);
    if (!itk::Math::IsPrime(sizeGreatestPrimeFactor))
    {
      std::cerr << "A prime number is expected for the greatest prime factor size!" << std::endl;
      return EXIT_FAILURE;
    }
    convoluter->SetSizeGreatestPrimeFactor(sizeGreatestPrimeFactor);
    ITK_TEST_SET_GET_VALUE(sizeGreatestPrimeFactor, convoluter->GetSizeGreatestPrimeFactor());
  }

  if (argc >= 6)
  {
    auto normalize = static_cast<bool>(std::stoi(argv[5]));
    convoluter->SetNormalize(normalize);
    ITK_TEST_SET_GET_VALUE(normalize, convoluter->GetNormalize());

    if (normalize)
    {
      convoluter->NormalizeOn();
      ITK_TEST_EXPECT_TRUE(convoluter->GetNormalize());
    }
    else
    {
      convoluter->NormalizeOff();
      ITK_TEST_EXPECT_TRUE(!convoluter->GetNormalize());
    }
  }

  if (argc >= 7)
  {
    std::string outputRegionMode(argv[6]);
    if (outputRegionMode == "SAME")
    {
      convoluter->SetOutputRegionMode(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME);
      ITK_TEST_SET_GET_VALUE(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME,
                             convoluter->GetOutputRegionMode());
    }
    else if (outputRegionMode == "VALID")
    {
      convoluter->SetOutputRegionMode(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID);
      ITK_TEST_SET_GET_VALUE(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID,
                             convoluter->GetOutputRegionMode());
    }
    else
    {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << std::endl;
      std::cerr << "Valid values are SAME or VALID." << std::endl;
      return EXIT_FAILURE;
    }

    if (outputRegionMode == "SAME")
    {
      convoluter->SetOutputRegionModeToSame();
      ITK_TEST_SET_GET_VALUE(ConvolutionFilterType::OutputRegionModeEnum::SAME, convoluter->GetOutputRegionMode());
    }
    else
    {
      convoluter->SetOutputRegionModeToValid();
      ITK_TEST_SET_GET_VALUE(ConvolutionFilterType::OutputRegionModeEnum::VALID, convoluter->GetOutputRegionMode());
    }
  }

  itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
  convoluter->SetBoundaryCondition(&constantBoundaryCondition);
  itk::PeriodicBoundaryCondition<ImageType>        periodicBoundaryCondition;
  itk::ZeroFluxNeumannBoundaryCondition<ImageType> zeroFluxNeumannBoundaryCondition;
  if (argc >= 7)
  {
    std::string boundaryCondition(argv[7]);
    if (boundaryCondition == "CONSTANT")
    {
      convoluter->SetBoundaryCondition(&constantBoundaryCondition);
      ITK_TEST_SET_GET_VALUE(&constantBoundaryCondition, convoluter->GetBoundaryCondition());
    }
    else if (boundaryCondition == "PERIODIC")
    {
      convoluter->SetBoundaryCondition(&periodicBoundaryCondition);
      ITK_TEST_SET_GET_VALUE(&periodicBoundaryCondition, convoluter->GetBoundaryCondition());
    }
    else if (boundaryCondition == "ZEROFLUXNEUMANN")
    {
      convoluter->SetBoundaryCondition(&zeroFluxNeumannBoundaryCondition);
      ITK_TEST_SET_GET_VALUE(&zeroFluxNeumannBoundaryCondition, convoluter->GetBoundaryCondition());
    }
    else
    {
      std::cerr << "Invalid BoundaryCondition '" << boundaryCondition << "'." << std::endl;
      std::cerr << "Valid values are CONSTANT, PERIODIC or ZEROFLUXNEUMANN." << std::endl;
      return EXIT_FAILURE;
    }
  }

  itk::SimpleFilterWatcher watcher(convoluter, "filter");

  ITK_TRY_EXPECT_NO_EXCEPTION(convoluter->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(convoluter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Test VALID output region mode with kernel that is larger than
  // the input image. Should result in a zero-size valid region.
  ImageType::Pointer    largeKernel = ImageType::New();
  ImageType::RegionType kernelRegion(reader1->GetOutput()->GetLargestPossibleRegion().GetSize());
  kernelRegion.PadByRadius(5);

  largeKernel->SetRegions(kernelRegion);
  largeKernel->Allocate();
  convoluter->SetOutputRegionModeToValid();
  convoluter->SetInput(reader1->GetOutput());
  convoluter->SetKernelImage(largeKernel);

  ITK_TRY_EXPECT_EXCEPTION(convoluter->Update());

  // Test for invalid request region.
  ImageType::IndexType invalidIndex;
  invalidIndex.Fill(1000);
  ImageType::SizeType invalidSize;
  invalidSize.Fill(1000);
  ImageType::RegionType invalidRequestRegion(invalidIndex, invalidSize);
  convoluter->GetOutput()->SetRequestedRegion(invalidRequestRegion);

  ITK_TRY_EXPECT_EXCEPTION(convoluter->Update());

  return EXIT_SUCCESS;
}
