/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMExceptionTest.cxx
  Language:  C++
  Date: $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMException.h"
#include "itkExceptionObject.h"
#include "itkFEM.h"
#include <iostream>
#include <exception>

using namespace std;
using namespace itk;
using namespace fem;

int itkFEMExceptionTest(int, char *[])
{
    try {
      throw FEMException(__FILE__,__LINE__, "itkFEMException");
    }
    catch (ExceptionObject &) {
      std::cout << "Exception caught\n";
    }

    try {
      throw FEMExceptionIO(__FILE__,__LINE__,"itkFEMExceptionIO","IO exception");
    }
    catch (ExceptionObject &) {
       std::cout << "IO exception caught\n";
    }

    try {
      throw FEMExceptionWrongClass(__FILE__,__LINE__,"itkFEMExceptionWrongClass");
    }
    catch (ExceptionObject &) {
      std::cout << "Wrong class exception caught\n";
    }

    try {
      throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"itkFEMExceptionObjectNotFound","baseClassName",0);
    }
    catch (ExceptionObject &) {
      std::cout << "Not found exception caught\n";
    }

    try {
      throw FEMExceptionSolution(__FILE__,__LINE__,"itkFEMExceptionSolution","Solution exception");
    }
    catch (ExceptionObject &) {
      std::cout << "Solution exception caught\n";
    }

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


