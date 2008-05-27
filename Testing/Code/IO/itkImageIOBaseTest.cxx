/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBaseTest.cxx
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
#include "itkImageIOBase.h"
#include "itkMetaImageIO.h"

int itkImageIOBaseTest( int , char * [] )
{
  typedef itk::ImageIOBase                 BaseReaderType;
  typedef BaseReaderType                   BaseReaderPointerType;

  typedef itk::MetaImageIO                 ReaderType;
  typedef ReaderType::Pointer              ReaderPointerType;

  itk::MetaImageIO::Pointer reader = itk::MetaImageIO::New();
  reader->SetNumberOfDimensions(3);

  bool gotException = false;
  try 
    {
    reader->SetDimensions(3,1);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDimensions"
              << std::endl;
    return EXIT_FAILURE;
    }
  
  gotException = false;
  try 
    {
    reader->SetOrigin(3,1.0);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetOrigin"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try 
    {
    reader->SetSpacing(3,1.0);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetSpacing"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try 
    {
    std::vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3,direction);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDirection"
              << std::endl;
    return EXIT_FAILURE;
    }

  gotException = false;
  try 
    {
    vnl_vector<double> direction(3);
    direction[0] = 1.0;
    direction[1] = 1.0;
    direction[2] = 1.0;
    reader->SetDirection(3,direction);
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Caught expected exception " << e << std::endl;
    gotException = true;
    }
  if (!gotException)
    {
    std::cerr << "Failed to catch expected exception in method SetDirection"
              << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
