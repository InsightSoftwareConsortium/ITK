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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryClosingByReconstructionImageFilter.h"
#include "itkTestingMacros.h"


int
itkBinaryClosingByReconstructionImageFilterTest(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName fullyConnected foregroundValue kernelSize" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using KernelType = itk::BinaryBallStructuringElement<bool, Dimension>;
  KernelType           ball;
  KernelType::SizeType ballSize;
  ballSize.Fill(std::stoi(argv[5]));
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  using FilterType = itk::BinaryClosingByReconstructionImageFilter<ImageType, KernelType>;
  FilterType::Pointer reconstructionFilter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reconstructionFilter, BinaryClosingByReconstructionImageFilter, KernelImageFilter);


  itk::SimpleFilterWatcher watcher(reconstructionFilter, "filter");

  auto fullyConnected = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(reconstructionFilter, FullyConnected, fullyConnected);

  typename FilterType::InputImagePixelType foregroundValue = std::stoi(argv[4]);
  reconstructionFilter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, reconstructionFilter->GetForegroundValue());

  reconstructionFilter->SetKernel(ball);

  reconstructionFilter->SetInput(reader->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(reconstructionFilter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
