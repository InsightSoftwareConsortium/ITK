/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkArrayTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>

#include "itkArray.h"


void Set_c_Array(int x[3])
{
  x[0] = 1;
  x[1] = 2;
  x[2] = 3;
}

void Set_ArrayReference(itk::Array<int, 3>::Reference x)
{
  x[0] = 1;
  x[1] = 2;
  x[2] = 3;
}

void Print_Array(itk::Array<int, 3> x, std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}

void Print_c_ArrayConst(const int x[3], std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}

void Print_ArrayConstReference(itk::Array<int, 3>::ConstReference x,
                               std::ostream& os)
{
  os << '{' << x[0] << ',' << x[1] << ',' << x[2] << '}' << std::endl;
}


int main(void)
{
  // Test out many combinations of using c-style arrays, Array,
  // Array::Reference, and Array::ConstReference.
  
  int c_Array1[3] = {0,0,0};
  
  Set_c_Array(c_Array1);
  Print_Array(c_Array1, std::cout);
  Print_ArrayConstReference(c_Array1, std::cout);
  Print_c_ArrayConst(c_Array1, std::cout);
  
  int c_Array2[3] = {0,0,0};
  Set_ArrayReference(c_Array2);
  Print_Array(c_Array2, std::cout);
  
  itk::Array<int, 3> array3;
  array3 = 4,4,4;
  Print_Array(array3, std::cout);
  Print_ArrayConstReference(array3, std::cout);
  Print_c_ArrayConst(array3, std::cout);

  Set_c_Array(array3);
  Print_Array(array3, std::cout);

  itk::Array<int, 3> array4;
  Set_ArrayReference(array4);
  Print_Array(array4, std::cout);
  
  itk::Array<int, 3>::Reference ref1 = array4;
  ref1 = 5,5,5;
  Print_Array(ref1, std::cout);
  Print_ArrayConstReference(ref1, std::cout);
  Print_c_ArrayConst(ref1, std::cout);
  Print_Array(array4, std::cout);
  
  itk::Array<int, 3>::Reference ref2 = c_Array1;
  ref2 = 6,6,6;
  Print_Array(c_Array1, std::cout);
  
  itk::Array<int, 3>::ConstReference cref3 = c_Array2;
  
  // Test out vnl_vector interface.
  std::cout << dot_product(array3.Get_vnl_vector(), ref1.Get_vnl_vector())
            << std::endl;  
  std::cout << dot_product(ref1.Get_vnl_vector(), ref2.Get_vnl_vector())
            << std::endl;
  
  return 0;
}
