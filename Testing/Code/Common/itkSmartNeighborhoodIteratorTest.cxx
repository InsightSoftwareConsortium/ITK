/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartNeighborhoodIteratorTest.cxx
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
#include "itkSmartNeighborhoodIterator.h"

int main()
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::SmartNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  itk::SmartNeighborhoodIterator<TestImageType>::IndexType zeroIDX;
  zeroIDX.Fill(0);
  
  itk::SmartNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;
  
  println("Creating SmartNeighborhoodIterator");
  itk::SmartNeighborhoodIterator<TestImageType>
    it(radius, img, img->GetRequestedRegion());

  println("Moving iterator using Superclass::SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Testing SetCenterPixel()");
  it.SetCenterPixel(zeroIDX);

  println("Testing SetPixel()");
  it.SetPixel(6,zeroIDX);

  println("Testing write out of bounds exception");
  it.GoToBegin();
  try
    {
      it.SetPixel(0, zeroIDX);
    }
  catch (itk::ExceptionObject &e)
    {
      println("OK");
      std::cout << e << std::endl;
      std::cout << e.GetDescription() << std::endl;
    }

  println("Using Superclass::GetNeighborhood()");
  itk::SmartNeighborhoodIterator<TestImageType>::NeighborhoodType n
    = it.GetNeighborhood();
  
  println("Testing SetNeighborhood()");
  it.SetNeighborhood(n);
  it.GetNeighborhood().Print(std::cout);

  println("Testing GetCenterPointer()");
  std::cout << it.GetCenterPointer() << " = "
            << *(it.GetCenterPointer()) << std::endl;

  return 0;
}
