/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArrayTest.cxx
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

#include <iostream>

#include "itkArray.h"

// Explicit instantiation to make sure all methods are compiled.
template class itk::Array<float>;



int itkArrayTest(int, char* [] )
{

  typedef itk::Array<float>  FloatArrayType;
  typedef itk::Array<double> DoubleArrayType;

  FloatArrayType  fa(10);
  DoubleArrayType da(10);

  return EXIT_SUCCESS;

}
