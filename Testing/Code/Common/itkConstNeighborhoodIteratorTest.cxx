/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstNeighborhoodIteratorTest.cxx
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

#include "itkNeighborhoodIteratorTestCommon.txx"
#include "itkConstNeighborhoodIterator.h"

int main()
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  itk::ConstNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  itk::ConstNeighborhoodIterator<TestImageType>::RegionType reg;
  itk::ConstNeighborhoodIterator<TestImageType>::SizeType sz;
  itk::ConstNeighborhoodIterator<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);
  
  println("Creating ConstNeighborhoodIterator");
  itk::ConstNeighborhoodIterator<TestImageType>
     it(radius, img, reg);

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Testing GetNeighborhood()");
  it.GetNeighborhood().Print(std::cout);

  println("Testing GetCenterPixel()");
  std::cout << it.GetCenterPixel() << std::endl;

  println("Testing GetCenterPointer()");
  std::cout << it.GetCenterPointer() << " = "
            << *(it.GetCenterPointer()) << std::endl;

  println("Testing GetPixel()");
  std::cout << it.GetPixel(6) << std::endl;

  println("Testing GetIndex()");
  std::cout << it.GetIndex() << std::endl;

  println("Testing GoToBegin()");
  it.GoToBegin();
  it.Print(std::cout);

  println("Testing IsAtBegin()");
  std::cout << it.IsAtBegin() << std::endl;

  println("Testing GoToEnd()");
  it.GoToEnd();
  it.Print(std::cout);

  println("Testing IsAtEnd()");
  std::cout << it.IsAtEnd() << std::endl;

  println("Testing forward iteration");
  it.GoToBegin();
  while (! it.IsAtEnd())
    {
      printnb<itk::ConstNeighborhoodIterator<TestImageType> >(it, false);
      ++it;
    }

  println("Testing reverse iteration");
  it.GoToEnd();
  while (! it.IsAtBegin())
    {
      --it;
      printnb<itk::ConstNeighborhoodIterator<TestImageType> >(it, false);
    }

  return 0;
}
