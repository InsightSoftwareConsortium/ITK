/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkNeighborhoodIteratorTestCommon.txx"
#include "itkConstNeighborhoodIterator.h"

int itkConstNeighborhoodIteratorTest(int, char* [] )
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

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);
  
  println("Testing GetNeighborhood()");
  it.GetNeighborhood().Print(std::cout);

  println("Printing neighborhood using GetPixel(i), GetPixel(offset) and GetIndex(i), and GetIndex(offset).");
  for (unsigned int j = 0; j < it.Size(); ++j)
    {
      std::cout << "GetOffset(" << j << ")=" << it.GetOffset(j);
      std::cout << " GetPixel(" << j << ")=" << it.GetPixel(j);
      std::cout << " GetPixel(" << it.GetOffset(j) << ")=" << it.GetPixel(it.GetOffset(j));
      std::cout << " GetIndex(" << j << ")=" << it.GetIndex(j) ;
      std::cout << " GetIndex(" << it.GetOffset(j) << ")=" << it.GetIndex(it.GetOffset(j));
      std::cout << std::endl;
    }
  
  println("Testing GetCenterPixel()");
  std::cout << it.GetCenterPixel() << std::endl;

  println("Testing GetCenterPointer()");
  std::cout << it.GetCenterPointer() << " = "
            << *(it.GetCenterPointer()) << std::endl;

  println("Testing GetIndex()");
  std::cout << it.GetIndex() << std::endl;

  println("Testing GetNext(3)");
  std::cout << it.GetNext(3) << std::endl;

  println("Testing GetNext(2)");
  std::cout << it.GetNext(2) << std::endl;

  println("Testing GetNext(1)");
  std::cout << it.GetNext(1) << std::endl;

  println("Testing GetNext(0) = GetNext(0,1)");
  std::cout << it.GetNext(0) << "=" << it.GetNext(0,1) <<  std::endl;

  println("Testing GetNext(0, 1)");
  std::cout << it.GetNext(0,1) << std::endl;

  println("Testing GetNext(1, 1)");
  std::cout << it.GetNext(1,1) << std::endl;

  println("Testing GetPrevious(3)");
  std::cout << it.GetPrevious(3) << std::endl;

  println("Testing GetPrevious(2)");
  std::cout << it.GetPrevious(2) << std::endl;

  println("Testing GetPrevious(1)");
  std::cout << it.GetPrevious(1) << std::endl;

  println("Testing GetPrevious(0) = GetPrevious(0,1)");
  std::cout << it.GetPrevious(0) << "=" << it.GetPrevious(0,1) <<  std::endl;

  println("Testing GetPrevious(0, 1)");
  std::cout << it.GetPrevious(0,1) << std::endl;

  println("Testing GetPrevious(1, 1)");
  std::cout << it.GetPrevious(1,1) << std::endl;  

  println("Testing GetBoundingBoxAsImageRegion");
  std::cout << it.GetBoundingBoxAsImageRegion() << std::endl;


  
  println("Testing random access iteration");

  TestImageType::Pointer ra_img = GetTestImage(10, 10, 5, 3);
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  println("Creating ConstNeighborhoodIterator");
  itk::ConstNeighborhoodIterator<TestImageType>
     ra_it(radius, ra_img, ra_img->GetRequestedRegion());

  println("Testing random access");
  ra_it.Begin();
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [1, 1, 1, 1]");
  OffsetType a_off;
  a_off.Fill(1);
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Subtracting [1, 1, 1, 1]");
  ra_it -= a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [0 0 0 2]");
  a_off.Fill(0);
  a_off[3] = 2;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [0 8 0 0]");
  a_off.Fill(0);
  a_off[1] = 8;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);

  println("Adding [5 -3 2 -1]");
  a_off[0] = 5;
  a_off[1] = -3;
  a_off[2] = 2;
  a_off[3] = -1;
  ra_it += a_off;
  printnb<itk::ConstNeighborhoodIterator<TestImageType> >(ra_it, false);


  

  
  return 0;
  
  
}
