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
#include "itkFFTShiftImageFilter.h"
#include "itkTestingMacros.h"

int
itkFFTShiftImageFilterTest(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage inverse" << std::endl;
    std::cerr << "  inputImage: The input image." << std::endl;
    std::cerr << "  outputImage: The output image." << std::endl;
    std::cerr << "  inverse: 0, to perform a forward transform, or 1 to perform" << std::endl;
    std::cerr << "           an inverse transform." << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 3;

  using PType = itk::RGBPixel<unsigned char>;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using FilterType = itk::FFTShiftImageFilter<IType, IType>;
  FilterType::Pointer filter = FilterType::New();
  // test default values
  if (filter->GetInverse() != false)
  {
    std::cerr << "Wrong default Inverse." << std::endl;
    return EXIT_FAILURE;
  }

  //
  // Tests for raising code coverage
  //
  try
  {
    filter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << excp << std::endl;
    std::cout << "catched EXPECTED exception for emtpy image as input" << std::endl;
  }

  filter->InverseOn();
  if (!filter->GetInverse())
  {
    std::cerr << "Set/GetInverse() error" << std::endl;
    return EXIT_FAILURE;
  }

  filter->InverseOff();
  if (filter->GetInverse())
  {
    std::cerr << "Set/GetInverse() error" << std::endl;
    return EXIT_FAILURE;
  }


  filter->SetInput(reader->GetOutput());

  filter->SetInverse(static_cast<bool>(std::stoi(argv[3])));
  if (filter->GetInverse() != static_cast<bool>(std::stoi(argv[3])))
  {
    std::cerr << "Set/Get Inverse problem." << std::endl;
    return EXIT_FAILURE;
  }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
