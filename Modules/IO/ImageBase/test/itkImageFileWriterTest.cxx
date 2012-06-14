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

#include "itkImageFileWriter.h"

int itkImageFileWriterTest(int ac, char* av[])
{

  if (ac < 2)
    {
    std::cout << "usage: itkIOTests itkImageFileWriterTest outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,2>               ImageNDType;
  typedef itk::ImageFileWriter<ImageNDType> WriterType;

  ImageNDType::Pointer image = ImageNDType::New();
  ImageNDType::RegionType region;
  ImageNDType::IndexType index;
  ImageNDType::SizeType size;


  size.Fill(5);
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions(region);
  image->Allocate();

  // Try an empty write
  int status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->Update();
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

  // Now try an image but no filename
  status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->Update();
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

  // Now try a write with an image but a bad output extension
  status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName("this_is_a_bad_filename");
    writer->Update();
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

  // Let's not be too negative. Try a write to a valid file.
  status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(av[1]);
    writer->Update();
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

  // Let's do the same with UpdateLargestPossibleRegion(), to make sure it does something
  status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(av[1]);
    writer->UpdateLargestPossibleRegion();
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
