/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMExceptionTest.cxx
  Language:  C++
  Date: $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkMacro.h"
#include "itkFEM.h"
#include <iostream>
#include <exception>


int itkFEMExceptionTest(int, char *[])
{
    try {
      throw itk::fem::FEMException(__FILE__,__LINE__, "itkFEMException");
    }
    catch (itk::ExceptionObject &) {
      std::cout << "Exception caught\n";
    }

    try {
      throw itk::fem::FEMExceptionIO(__FILE__,__LINE__,"itkFEMExceptionIO","IO exception");
    }
    catch (itk::ExceptionObject &) {
       std::cout << "IO exception caught\n";
    }

    try {
      throw itk::fem::FEMExceptionWrongClass(__FILE__,__LINE__,"itkFEMExceptionWrongClass");
    }
    catch (itk::ExceptionObject &) {
      std::cout << "Wrong class exception caught\n";
    }

    try {
      throw itk::fem::FEMExceptionObjectNotFound(__FILE__,__LINE__,"itkFEMExceptionObjectNotFound","baseClassName",0);
    }
    catch (itk::ExceptionObject &) {
      std::cout << "Not found exception caught\n";
    }

    try {
      throw itk::fem::FEMExceptionSolution(__FILE__,__LINE__,"itkFEMExceptionSolution","Solution exception");
    }
    catch (itk::ExceptionObject &) {
      std::cout << "Solution exception caught\n";
    }

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


