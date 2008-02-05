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

  WriterType::Pointer writer = WriterType::New();
 
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
  writer->SetInput(image);
  status = 1;
  try
    {
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
  writer->SetFileName("this_is_a_bad_filename");
  status = 1;
  try
    {
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

  // Now try a write with an image but a filename that cannot be created
  writer->SetFileName("/this_is_a_bad_directory/and_a_bad_filename.png");
  status = 1;
  try
    {
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
  // Let's not be too negative. Try a write to a valid file.
  writer->SetFileName(av[1]);
  status = 1;
  try
    {
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
  writer->Print(std::cout);

  return EXIT_SUCCESS;

}
