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

void Set_ArrayReference(itk::FixedArray<int, 3>::Reference x)
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

void Print_ArrayConstReference(itk::FixedArray<int, 3>::ConstReference x,
                               std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}

void Print_Array5(itk::FixedArray<int, 5> x, std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << ','
     << x[3] << ',' << x[4] << '}' << std::endl;
}

int main(void)
{
  // Test out many combinations of using c-style arrays, FixedArray,
  // FixedArray::Reference, and FixedArray::ConstReference.
  
  int c_Array1[3] = {0,0,0};
  
  Set_c_Array(c_Array1);
  Print_Array(c_Array1, std::cout);
  Print_ArrayConstReference(c_Array1, std::cout);
  Print_c_ArrayConst(c_Array1, std::cout);
  
  int c_Array2[3] = {0,0,0};
  Set_ArrayReference(c_Array2);
  Print_Array(c_Array2, std::cout);
  
  int array3Init[3] = {4,4,4};
  itk::FixedArray<int, 3> array3 = array3Init;
  Print_Array(array3, std::cout);
  Print_ArrayConstReference(array3, std::cout);
  Print_c_ArrayConst(array3.GetDataPointer(), std::cout);

  Set_c_Array(array3.GetDataPointer());
  Print_Array(array3, std::cout);

  itk::FixedArray<int, 3> array4;
  Set_ArrayReference(array4);
  Print_Array(array4, std::cout);
  
  itk::FixedArray<int, 3>::Reference ref1 = array4;
  int arrayRefWrite[3] = {5,5,5};
  ref1 = arrayRefWrite;
  Print_Array(ref1, std::cout);
  Print_ArrayConstReference(ref1.GetDataPointer(), std::cout);
  Print_c_ArrayConst(ref1.GetDataPointer(), std::cout);
  Print_Array(array4, std::cout);
  
  itk::FixedArray<int, 3>::Reference ref2 = c_Array1;
  int arrayRefWrite2[3] = {6,6,6};
  ref2 = arrayRefWrite2;
  Print_Array(c_Array1, std::cout);
  
  itk::FixedArray<int, 3>::ConstReference cref3 = c_Array2;
  Print_ArrayConstReference(cref3, std::cout);
  
  // Test range selection.
  itk::FixedArray<int, 5> array5;
  array5.Fill(0);
  array5[itk::Range<1,3>()] = ref2;
  Print_Array5(array5, std::cout);
  
  Print_Array(array5[itk::Range<2,4>()], std::cout);
  
  const itk::FixedArray<int, 5>::Reference ref4 = array5;
  Print_Array(ref4[itk::Range<1,3>()], std::cout);
  
  itk::FixedArray<int, 4>::ConstReference ref5 = array5[itk::Range<1,4>()];
  Print_Array(ref5[itk::Range<1,3>()], std::cout);
  
  // Test operator!= and operator==
  if ( array4 != array4 ) return 1; //should be equal
  if ( array4 != ref1 ) return 1; //should be equal
  if ( !(array4 != ref2) ) return 1; //should not be equal
  if ( !(array4 == array4) ) return 1; //should be equal
  if ( !(array4 == ref1) ) return 1; //should be equal
  if ( array4 == ref2 ) return 1; //should not be equal

  return 0;
}
