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

#include "itkBlackTopHatImageFilter.h"
#include "itkWhiteTopHatImageFilter.h"
#include "itkMathematicalMorphologyEnums.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


template <typename TKernelImageFilter>
int
itkTopHatImageFilterTestHelper(TKernelImageFilter *                              filter,
                               typename TKernelImageFilter::KernelType           ball,
                               const bool                                        safeBorder,
                               const itk::MathematicalMorphologyEnums::Algorithm algorithm,
                               const bool                                        forceAlgorithm,
                               const char *                                      inputFileName,
                               const char *                                      outputFileName)
{
  // Declare the reader and writer
  using ReaderType = itk::ImageFileReader<typename TKernelImageFilter::InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputFileName);

  itk::SimpleFilterWatcher watcher(filter, "filter");

  ITK_TEST_SET_GET_BOOLEAN(filter, SafeBorder, safeBorder);

  filter->SetAlgorithm(algorithm);
  ITK_TEST_SET_GET_VALUE(algorithm, filter->GetAlgorithm());

  ITK_TEST_SET_GET_BOOLEAN(filter, ForceAlgorithm, forceAlgorithm);

  // Connect the structuring element
  filter->SetKernel(ball);

  // Connect the pipeline
  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Write output
  using WriterType = itk::ImageFileWriter<typename TKernelImageFilter::OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputFileName);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}


int
itkTopHatImageFilterTest(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " outputImage"
              << " 0/1(Black/White)"
              << " radius"
              << " safeBorder"
              << " algorithm"
              << " forceAlgorithm" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  // Define the pixel type
  using PixelType = unsigned char;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  // Declare the type for the structuring element
  using KernelType = itk::BinaryBallStructuringElement<PixelType, Dimension>;

  const char * inputFileName = argv[1];
  const char * outputFileName = argv[2];

  // Create the structuring element
  KernelType           ball;
  KernelType::SizeType ballSize;
  ballSize[0] = std::stoi(argv[4]);
  ballSize[1] = std::stoi(argv[4]);
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  auto                                        safeBorder = static_cast<bool>(std::stoi(argv[5]));
  itk::MathematicalMorphologyEnums::Algorithm algorithm;
  auto                                        forceAlgorithm = static_cast<bool>(std::stoi(argv[7]));

  int testStatus = EXIT_SUCCESS;

  switch (std::stoi(argv[3]))
  {
    case 0:
    {
      // Create the filter
      using BlackFilterType = itk::BlackTopHatImageFilter<ImageType, ImageType, KernelType>;
      auto filter = BlackFilterType::New();

      ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BlackTopHatImageFilter, KernelImageFilter);


      algorithm = static_cast<BlackFilterType::AlgorithmEnum>(std::stoi(argv[6]));
      testStatus = itkTopHatImageFilterTestHelper<BlackFilterType>(
        filter, ball, safeBorder, algorithm, forceAlgorithm, inputFileName, outputFileName);

      break;
    }
    case 1:
    {
      // Create the filter
      using WhiteFilterType = itk::WhiteTopHatImageFilter<ImageType, ImageType, KernelType>;
      auto filter = WhiteFilterType::New();

      ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, WhiteTopHatImageFilter, KernelImageFilter);


      algorithm = static_cast<WhiteFilterType::AlgorithmEnum>(std::stoi(argv[6]));
      testStatus = itkTopHatImageFilterTestHelper<WhiteFilterType>(
        filter, ball, safeBorder, algorithm, forceAlgorithm, inputFileName, outputFileName);

      break;
    }
    default:
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Invalid filter selector: " << std::stoi(argv[3]) << std::endl;
      testStatus = EXIT_FAILURE;
  }

  return testStatus;
}
