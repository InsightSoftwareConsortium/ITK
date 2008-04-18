/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriterTest.cxx
  Language:  C++
  Date:      $Date$xgoto-l

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImage.h"
#include "itkImageFileWriter.h"

int itkImageFileWriterTest(int ac, char* av[])
{

  if (ac < 2)
    {
    std::cout << "usage: itkIOTests itkImageFileWriterTest outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,2> ImageNDType;
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

#if 0
  // Now try a write with an image but a filename that cannot be created
  status = 1;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName("/this_is_a_bad_directory/and_a_bad_filename.tif");
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
#endif
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


  return EXIT_SUCCESS;

}
