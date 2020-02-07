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
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " input output conn fg kernelSize" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
  }

  constexpr int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using KernelType = itk::BinaryBallStructuringElement<bool, dim>;
  KernelType           ball;
  KernelType::SizeType ballSize;
  ballSize.Fill(std::stoi(argv[5]));
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  using I2LType = itk::BinaryClosingByReconstructionImageFilter<IType, KernelType>;
  I2LType::Pointer reconstruction = I2LType::New();
  reconstruction->SetInput(reader->GetOutput());
  reconstruction->SetKernel(ball);
  reconstruction->SetFullyConnected(std::stoi(argv[3]));
  reconstruction->SetForegroundValue(std::stoi(argv[4]));
  //   reconstruction->SetBackgroundValue( std::stoi(argv[6]) );
  itk::SimpleFilterWatcher watcher(reconstruction, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(reconstruction->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();
  return 0;
}
