/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeTransformWriteAndReadTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformWriter.h"
#include "itkCompositeTransformReader.h"
#include "itkArray2D.h"


/* A simple test for CompositeTransformReader on its own. See
 * CompositeTransformWriterAndReaderTest for a test that checks
 * actual writing and then reading for errors. */

int itkCompositeTransformReaderTest(int , char *[] )
{
  const unsigned int NDimensions = 2;

  typedef itk::CompositeTransformReader<double, NDimensions> ReaderType;
  ReaderType::Pointer                   reader = ReaderType::New();

  /* Check for error if try to read w/out setting filename */
  bool caughtException = false;
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    caughtException = true;
    std::cout << "Exception caught as expected:" << std::endl;
    std::cout << ex << std::endl;
    }
  if( !caughtException )
    {
    std::cout << "No error generated for no filename." << std::endl;
    return EXIT_FAILURE;
    }

  /* Try with bad filename */
  try
    {
    reader->SetFileName( "junkfile!@#$%^&*()" );
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "Unexpected Exception caught:" << std::endl;
    std::cout << ex << std::endl;
    }

  caughtException = false;
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    caughtException = true;
    std::cout << "Exception caught as expected:" << std::endl;
    std::cout << ex << std::endl;
    }
  if( !caughtException )
    {
    std::cout << "No error generated for bad filename." << std::endl;
    return EXIT_FAILURE;
    }

  //Print
  std::cout << reader;

  return EXIT_SUCCESS;
}
