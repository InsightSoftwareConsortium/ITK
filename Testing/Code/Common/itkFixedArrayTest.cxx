/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedArrayTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkFixedArray.h"

// Explicit instantiation to make sure all methods are compiled.
template class itk::FixedArray<float, 3>;

void Set_c_Array(int x[3])
{
  x[0] = 1;
  x[1] = 2;
  x[2] = 3;
}

void Print_Array(itk::FixedArray<int, 3> x, std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}

void Print_c_ArrayConst(const int x[3], std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}

void Print_Array5(itk::FixedArray<int, 5> x, std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << ','
     << x[3] << ',' << x[4] << '}' << std::endl;
}

int itkArrayTest(int, char**)
{
  // Test out many combinations of using c-style arrays and FixedArray
  
  int c_Array1[3] = {0,0,0};
  
  Set_c_Array(c_Array1);
  Print_Array(c_Array1, std::cout);
  Print_c_ArrayConst(c_Array1, std::cout);

  int c_Array2[3] = {0,0,0};
  Print_Array(c_Array2, std::cout);
  
  int array3Init[3] = {4,4,4};
  itk::FixedArray<int, 3> array3 = array3Init;
  Print_Array(array3, std::cout);
  Print_c_ArrayConst(array3.GetDataPointer(), std::cout);

  Set_c_Array(array3.GetDataPointer());
  Print_Array(array3, std::cout);

  itk::FixedArray<int, 3> array4;
  Print_Array(array4, std::cout);
  
  // Test operator!= and operator==
  if ( array4 != array4 ) return 1; //should be equal
  if ( !(array4 == array4) ) return 1; //should be equal

  return 0;
}
