/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThreadDefsTest.cxx
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

#include "itkObject.h"
#include <vector>

int itkThreadDefsTest (int, char* [] )
{
#if defined(_NOTHREADS) && defined(ITK_USE_PTHREADS)
  std::cout << "ERROR: _NOTHREADS is defined and ITK_USE_PTHREADS is defined." << std::endl;
  std::cout << "STL containers WILL NOT BE thread safe on SGI's and GNU c++ systems." << std::endl;
    std::cout << "The C++ compiler needs a -D_PTHREADS option." << std::endl;
  return EXIT_FAILURE;
#endif
  
  return EXIT_SUCCESS;
}
