/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstRandomAccessNeighborhoodIteratorTest.cxx
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
