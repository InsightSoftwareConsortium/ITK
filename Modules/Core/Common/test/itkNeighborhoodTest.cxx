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

#include "itkNeighborhood.h"
#include "itkVector.h"


static void println(const char *s)
{
  std::cout << std::endl << s << std::endl;
}

int itkNeighborhoodTest(int, char* [] )
{
  unsigned int i;

  println("TESTING WITH VNL_VECTOR ALLOCATOR");
  itk::Neighborhood<float, 2, vnl_vector<float> > b;
  b.SetRadius(3);

  println("Test data access");
  for (i=0; i<b.Size(); ++i) b[i] = (float)i;
  b.Print(std::cout);

  println("Test non_const iterators");
  for (itk::Neighborhood<float, 2>::Iterator it = b.Begin();
       it < b.End(); ++it) *it = 4.0f;
  b.Print(std::cout);

  println("Test const_iterators");
  for (itk::Neighborhood<float, 2>::ConstIterator itc = b.Begin();
       itc < b.End(); ++itc) std::cout << *itc << " ";

  println("Copy the buffer into a vnl_vector");
  vnl_vector<float> v = b.GetBufferReference();
  v[2] = 0.0f;
  std::cout << &v << std::endl;
  b.Print(std::cout); // b unmodified

  println("Pointer to the buffer with a const vnl_vector");
  const vnl_vector<float> &vcp = b.GetBufferReference();
  std::cout << &vcp << std::endl;
  std::cout << vcp;

  println("Point to the buffer using a vnl_vector");
  vnl_vector<float> &vp = b.GetBufferReference();
  std::cout << &vp << std::endl;
  vp[2] = 0.0f;
  b.Print(std::cout); // b modified

  println("TESTING WITH DEFAULT ALLOCATOR");
  itk::Neighborhood<float, 2> q;
  itk::Size<2> rad;
  rad[0] = 3;
  rad[1] = 2;
  q.SetRadius(rad);
  q.Print(std::cout);

  println("Testing assignment operator");
  itk::Neighborhood<float, 2> p = q;
  p.Print(std::cout);

  println("Testing copy constructor");
  itk::Neighborhood<float, 2> r(q);
  r.Print(std::cout);

  println("Testing instantiation with itk::Vector");
  itk::Neighborhood< itk::Vector<float, 3>, 2 > s;
  s.SetRadius(rad);
  s.Print(std::cout);

  println("Testing stride lengths");
  itk::Neighborhood<float , 2> stride_2d;
  stride_2d.SetRadius(1);
  stride_2d.Print(std::cout);

  println("3d");
  itk::Neighborhood<float, 3> stride_3d;
  stride_3d.SetRadius(1);
  stride_3d.Print(std::cout);

  println("Printing the indices of the 8 neighbors");
  std::cout << stride_3d.GetOffset(4) << std::endl;
  std::cout << stride_3d.GetOffset(10)  << std::endl;
  std::cout << stride_3d.GetOffset(12)  << std::endl;
  std::cout << stride_3d.GetOffset(13)  << std::endl;
  std::cout << stride_3d.GetOffset(14)  << std::endl;
  std::cout << stride_3d.GetOffset(16)  << std::endl;
  std::cout << stride_3d.GetOffset(22)  << std::endl;


  println("ANOTHER TEST");
  itk::Neighborhood<float, 2> q2;
  q2.SetRadius(3);
  q2.Print(std::cout);

  return EXIT_SUCCESS;
}
