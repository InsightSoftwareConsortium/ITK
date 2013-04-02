/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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


int itkImageFileReaderTest1(int ac, char* av[])
{

  if (ac < 1)
    {
    std::cout << "usage: ITKImageIOBaseTestDriver itkImageFileReaderTest" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,2>               ImageNDType;
  typedef itk::ImageFileReader<ImageNDType> ReaderType;

  // Try an empty read
  int status = 1;
  try
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
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


  // Now try a read with an image that doesn't exist
  status = 1;
  try
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName("this_file_should_not_exist");
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
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

  // Let's try to read a file where no ImageIO can read it
  status = 1;
  try
    {
    ReaderType::Pointer reader = ReaderType::New();
    // this is the executable and no reader should be able to read it
    reader->SetFileName(av[0]);
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
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
