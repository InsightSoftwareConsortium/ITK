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

#include "itkConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkConvolutionImageFilterBase.h"

int
itkConvolutionImageFilterTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputImage kernelImage outputImage [normalizeImage]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);
  reader1->Update();

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);
  reader2->Update();

  using ConvolutionFilterType = itk::ConvolutionImageFilter<ImageType>;
  auto convoluter = ConvolutionFilterType::New();
  convoluter->SetInput(reader1->GetOutput());
  convoluter->SetKernelImage(reader2->GetOutput());

  itk::SimpleFilterWatcher watcher(convoluter, "filter");

  if (argc >= 5)
  {
    convoluter->SetNormalize(static_cast<bool>(std::stoi(argv[4])));
  }

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(convoluter->GetOutput());


  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


  //
  // Tests for raising code coverage
  //
  convoluter->Print(std::cout);

  auto emtpyImage = ImageType::New();
  convoluter->SetInput(emtpyImage);
  try
  {
    convoluter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << excp << std::endl;
    std::cout << "caught EXPECTED exception for empty image as input" << std::endl;
  }

  convoluter->NormalizeOn();
  if (!convoluter->GetNormalize())
  {
    std::cerr << "Set/GetNormalize() error" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->NormalizeOff();
  if (convoluter->GetNormalize())
  {
    std::cerr << "Set/GetNormalize() error" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetNormalize(true);
  if (!convoluter->GetNormalize())
  {
    std::cerr << "Set/GetNormalize() error" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetNormalize(false);
  if (convoluter->GetNormalize())
  {
    std::cerr << "Set/GetNormalize() error" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetOutputRegionMode(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME);
  if (convoluter->GetOutputRegionMode() !=
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME)
  {
    std::cerr << "SetOutputRegionMode() error when argument is SAME" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetOutputRegionMode(itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID);
  if (convoluter->GetOutputRegionMode() !=
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID)
  {
    std::cerr << "SetOutputRegionMode() error when argument is VALID" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetOutputRegionModeToSame();
  if (convoluter->GetOutputRegionMode() !=
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME)
  {
    std::cerr << "SetOutputRegionModeToSame() error" << std::endl;
    return EXIT_FAILURE;
  }

  convoluter->SetOutputRegionModeToValid();
  if (convoluter->GetOutputRegionMode() !=
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID)
  {
    std::cerr << "SetOutputRegionModeToValid() error" << std::endl;
    return EXIT_FAILURE;
  }

  itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
  convoluter->SetBoundaryCondition(&constantBoundaryCondition);
  if (convoluter->GetBoundaryCondition() != &constantBoundaryCondition)
  {
    std::cerr << "SetBoundaryCondition() error" << std::endl;
    return EXIT_FAILURE;
  }

  // Test VALID output region mode with kernel that is larger than
  // the input image. Should result in a zero-size valid region.
  auto                  largeKernel = ImageType::New();
  ImageType::RegionType kernelRegion(reader1->GetOutput()->GetLargestPossibleRegion().GetSize());
  kernelRegion.PadByRadius(5);

  largeKernel->SetRegions(kernelRegion);
  largeKernel->Allocate();
  convoluter->SetOutputRegionModeToValid();
  convoluter->SetInput(reader1->GetOutput());
  convoluter->SetKernelImage(largeKernel);
  try
  {
    convoluter->Update();
    std::cerr << "Failed to catch expected exception when kernel is larger than the input image." << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Caught expected exception when kernel is larger than the input image." << std::endl;
    std::cout << e << std::endl;
  }

  // Test for invalid request region.
  ImageType::IndexType invalidIndex;
  invalidIndex.Fill(1000);
  ImageType::SizeType invalidSize;
  invalidSize.Fill(1000);
  ImageType::RegionType invalidRequestRegion(invalidIndex, invalidSize);
  convoluter->GetOutput()->SetRequestedRegion(invalidRequestRegion);
  try
  {
    convoluter->Update();
    std::cerr << "Failed to catch expected exception when request region is outside the largest "
              << "possible region." << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Caught expected exception when request region is outside the largest "
              << "possible region." << std::endl;
    std::cout << e << std::endl;
  }

  // Test streaming enumeration for ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion elements
  const std::set<itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion>
    allConvolutionImageFilterOutputRegion{
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME,
      itk::ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::VALID
    };
  for (const auto & ee : allConvolutionImageFilterOutputRegion)
  {
    std::cout << "STREAMED ENUM VALUE ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion: " << ee
              << std::endl;
  }

  return EXIT_SUCCESS;
}
