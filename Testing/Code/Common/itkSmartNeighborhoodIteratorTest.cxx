/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartNeighborhoodIteratorTest.cxx
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

  // Test GetBoundaryCondition

  it.GetBoundaryCondition();

  
  return 0;
}
