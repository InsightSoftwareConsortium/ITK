/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstRandomAccessNeighborhoodIteratorTest.cxx
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
#include "itkConstRandomAccessNeighborhoodIterator.h"

int main()
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstRandomAccessNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  itk::ConstRandomAccessNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  println("Creating ConstRandomAccessNeighborhoodIterator");
  itk::ConstRandomAccessNeighborhoodIterator<TestImageType>
     it(radius, img, img->GetRequestedRegion());

  println("Testing random access");
  it.Begin();
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  println("Adding [1, 1, 1, 1]");
  OffsetType a_off;
  a_off.Fill(1);
  it += a_off;
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  println("Subtracting [1, 1, 1, 1]");
  it -= a_off;
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  println("Adding [0 0 0 2]");
  a_off.Fill(0);
  a_off[3] = 2;
  it += a_off;
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  println("Adding [0 8 0 0]");
  a_off.Fill(0);
  a_off[1] = 8;
  it += a_off;
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  println("Adding [5 -3 2 -1]");
  a_off[0] = 5;
  a_off[1] = -3;
  a_off[2] = 2;
  a_off[3] = -1;
  it += a_off;
  printnb<itk::ConstRandomAccessNeighborhoodIterator<TestImageType> >(it, false);

  return 0;
}
