/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstSmartNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkNeighborhoodIteratorTestCommon.txx"
#include "itkConstSmartNeighborhoodIterator.h"

int itkConstSmartNeighborhoodIteratorTest(int, char**)
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstSmartNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  itk::ConstSmartNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  println("Creating ConstNeighborhoodIterator");
  itk::ConstSmartNeighborhoodIterator<TestImageType>
     it(radius, img, img->GetRequestedRegion());

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
      printnb<itk::ConstSmartNeighborhoodIterator<TestImageType> >(it, false);
      ++it;
    }

  println("Testing reverse iteration");
  it.GoToEnd();
  while (! it.IsAtBegin())
    {
      --it;
      printnb<itk::ConstSmartNeighborhoodIterator<TestImageType> >(it, false);
    }

  // Test GetBoundaryCondition

  it.GetBoundaryCondition();
  
  return 0;
}
