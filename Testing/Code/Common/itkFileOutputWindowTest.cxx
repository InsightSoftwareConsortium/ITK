/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileOutputWindowTest.cxx
  Language:  C++
  Date:      $Date$
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

#include "itkFileOutputWindow.h"

#include <iostream>
#include <string>


int itkFileOutputWindowTest(int, char* [] )
{
  
  // Declare the type for the morphology Filter
  typedef itk::FileOutputWindow myFileOutputWindow;
  
  // Create the filter
  myFileOutputWindow::Pointer window = myFileOutputWindow::New();

  // Test itkSetStringMacro
  const char * fileName = "";
  window->SetFileName(fileName);
  
  // Test itkGetStringMacro
  const char * fileName2 = window->GetFileName();
  std::cout << "window->GetFileName(): " << fileName2 << std::endl;

  // Test itkSetMacros
  const bool flush = false;
  window->SetFlush(flush);
  const bool append = false;
  window->SetAppend(append);

  // Test itkGetMacros
  bool flush2 = window->GetFlush();
  std::cout << "window->GetFlush(): " << flush2 << std::endl;
  bool append2 = window->GetAppend();
  std::cout << "window->GetAppend(): " << append2 << std::endl;

  // Test itkBooleanMacros
  window->FlushOn();
  window->AppendOn();

  // Test 

  return 0;  
}

