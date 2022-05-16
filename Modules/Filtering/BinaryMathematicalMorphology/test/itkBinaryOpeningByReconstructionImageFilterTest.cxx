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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryOpeningByReconstructionImageFilter.h"
#include "itkTestingMacros.h"


int
itkBinaryOpeningByReconstructionImageFilterTest(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output kernelSize conn foregroundValue backgroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;
  using PixelType = unsigned char;

  using IType = itk::Image<PixelType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using KernelType = itk::BinaryBallStructuringElement<bool, dim>;
  KernelType           ball;
  KernelType::SizeType ballSize;
  ballSize.Fill(std::stoi(argv[3]));
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  using I2LType = itk::BinaryOpeningByReconstructionImageFilter<IType, KernelType>;
  auto reconstruction = I2LType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reconstruction, BinaryOpeningByReconstructionImageFilter, KernelImageFilter);


  reconstruction->SetInput(reader->GetOutput());
  reconstruction->SetKernel(ball);

  auto fullyConnected = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(reconstruction, FullyConnected, fullyConnected);

  auto foregroundValue = static_cast<typename I2LType::PixelType>(std::stoi(argv[5]));
  reconstruction->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, reconstruction->GetForegroundValue());

  auto backgroundValue = static_cast<typename I2LType::PixelType>(std::stoi(argv[6]));
  reconstruction->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, reconstruction->GetBackgroundValue());

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(reconstruction->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
