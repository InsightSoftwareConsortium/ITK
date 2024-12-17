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

#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


int
itkImageFileWriterTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName " << std::endl;
    return EXIT_FAILURE;
  }

  using ImageNDType = itk::Image<short, 2>;
  using WriterType = itk::ImageFileWriter<ImageNDType>;

  auto                             image = ImageNDType::New();
  constexpr ImageNDType::IndexType index{};
  auto                             size = itk::MakeFilled<ImageNDType::SizeType>(5);

  const ImageNDType::RegionType region{ index, size };

  image->SetRegions(region);
  image->Allocate();

  // Try an empty write
  int status = 1;
  try
  {
    auto writer = WriterType::New();
    writer->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
  }
  if (status)
  {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }

  // Now try an image but no filename
  status = 1;
  try
  {
    auto writer = WriterType::New();
    writer->SetInput(image);
    writer->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
  }
  if (status)
  {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }

  // Now try a write with an image but a bad output extension
  status = 1;
  try
  {
    auto writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName("this_is_a_bad_filename");
    writer->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
  }
  if (status)
  {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }

  // Let's not be too negative. Try a write to a valid file.
  status = 1;
  try
  {
    auto writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(argv[1]);
    writer->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
  }
  if (status)
  {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }

  // Let's do the same with UpdateLargestPossibleRegion(), to make sure it does something
  status = 1;
  try
  {
    auto writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(argv[1]);
    writer->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
  }
  if (status)
  {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
