/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkNeighborhood.h"
#include "itkVector.h"


inline void println(const char *s)
{
  std::cout << std::endl << s << std::endl;
}

int main()
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
  
  
  
  return 0;
}
