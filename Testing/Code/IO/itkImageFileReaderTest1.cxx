/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReaderTest1.cxx
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
#include "itkImageFileReader.h"


int itkImageFileReaderTest1(int ac, char* av[])
{

  if (ac < 1)
    {
    std::cout << "usage: itkIOTests itkImageFileReaderTest" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,2> ImageNDType;
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
