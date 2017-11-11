/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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

int itkFixedArrayTest(int, char* [] )
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
  array4.Fill(0);
  Print_Array(array4, std::cout);

  // Test operator!= and operator==
  if ( array4 != array4 ) return EXIT_FAILURE; //should be equal
  if ( !(array4 == array4) ) return EXIT_FAILURE; //should be equal

  // Test Get/Set element
  const unsigned int n = 20;
  itk::FixedArray< unsigned int, n > array20;
  for(unsigned int i=0; i<n; i++)
    {
    array20.SetElement(i,i);
    }

  for(unsigned int k=0; k<n; k++)
    {
    if( array20.GetElement(k) != k )
      {
      std::cerr << "Set/Get element test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "FixedArray<unsigned int, 20>::Iterator it = array20.Begin();" << std::endl;
  itk::FixedArray<unsigned int, 20>::Iterator it = array20.Begin();
  while (it != array20.End())
    {
    std::cout << *it << std::endl;
    ++it;
    }

  std::cout << "FixedArray<unsigned int, 20>::Iterator it = array20.End();" << std::endl;
  itk::FixedArray<unsigned int, 20>::Iterator bit = array20.End();
  while (--bit >= array20.Begin())
    {
    std::cout << *bit << std::endl;
    }

  std::cout << "FixedArray<unsigned int, 20>::ConstIterator it = array20.Begin();" << std::endl;
  itk::FixedArray<unsigned int, 20>::ConstIterator cit = array20.Begin();
  while (cit != array20.End())
    {
    std::cout << *cit << std::endl;
    ++cit;
    }

  std::cout << "FixedArray<unsigned int, 20>::ReverseIterator rit = array20.rBegin();" << std::endl;
  itk::FixedArray<unsigned int, 20>::ReverseIterator rit = array20.rBegin();
  while (rit != array20.rEnd())
    {
    std::cout << *rit << std::endl;
    ++rit;
    }

  std::cout << "FixedArray<unsigned int, 20>::ConstReverseIterator crit = array20.rBegin();" << std::endl;
  itk::FixedArray<unsigned int, 20>::ConstReverseIterator crit = array20.rBegin();
  while (crit != array20.rEnd())
    {
    std::cout << *crit << std::endl;
    ++crit;
    }

  // Try all index types
#define TRY_INDEX_CONST(T) {T in = 10; if (array20[in] != 10) {std::cerr << "index failed" << std::endl; return EXIT_FAILURE;}}
      TRY_INDEX_CONST(short);
      TRY_INDEX_CONST(unsigned short);
      TRY_INDEX_CONST(int);
      TRY_INDEX_CONST(unsigned int);
      TRY_INDEX_CONST(long);
      TRY_INDEX_CONST(unsigned long);
      TRY_INDEX_CONST(long long);
      TRY_INDEX_CONST(unsigned long long);
#define TRY_INDEX(T) {T in = 10; array20[in] = 10;}
      TRY_INDEX(short);
      TRY_INDEX(unsigned short);
      TRY_INDEX(int);
      TRY_INDEX(unsigned int);
      TRY_INDEX(long);
      TRY_INDEX(unsigned long);
      TRY_INDEX(long long);
      TRY_INDEX(unsigned long long);
  return EXIT_SUCCESS;
}
