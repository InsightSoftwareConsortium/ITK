/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageReverseIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionReverseIterator.h"


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <class T, unsigned int VImageDimension>
void TestConstPixelAccess(const itk::Image<T, VImageDimension> &in,
                          itk::Image<T, VImageDimension> &out)
{
  typename itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = {{5, 10, 15}};
  typename itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = {{8, 15, 17}};

  T vec;
  
  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int itkImageReverseIteratorTest(int, char* [] )
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  float origin3D[3] = { 5, 2.1, 8.1};
  float spacing3D[3] = { 1.5, 2.1, 1};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType imageSize3D = {{ 20, 40, 60 }};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType bufferSize3D = {{ 8, 20, 14 }};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType regionSize3D = {{ 4,  6,  6 }};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType startIndex3D = {{5, 4, 1}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType bufferStartIndex3D = {{2, 3, 5}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionStartIndex3D = {{5, 10, 12}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionEndIndex3D = {{8, 15, 17}};


  itk::Image<itk::Vector<unsigned short, 5>, 3>::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
  o3->SetLargestPossibleRegion( region );
  region.SetSize(bufferSize3D);
  region.SetIndex(bufferStartIndex3D);
  o3->SetBufferedRegion( region );
  region.SetSize(regionSize3D);
  region.SetIndex(regionStartIndex3D);
  o3->SetRequestedRegion( region );
  
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;
  
  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);

  
  itk::ImageIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > standardIt(o3, region);

  // Iterate over a region using a simple for loop
  itk::ImageRegionIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > it(o3, region);

  for ( ; !it.IsAtEnd(); ++it)
    {
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    std::cout << "Simple iterator loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Iterator over the region backwards using a simple for loop
  itk::ImageRegionIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > backIt(o3, region);
 
  backIt = backIt.End(); // one pixel past the end of the region
  do
    {
    --backIt;

    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = backIt.GetIndex();
    std::cout << "Simple iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;    
    }
  while (!backIt.IsAtBegin()); // stop when we reach the beginning
  

  // Iterate over a region backwards using a reverse iterator
  itk::ImageRegionReverseIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > reverseIt(o3, region);

  for ( ; !reverseIt.IsAtEnd(); ++reverseIt)
    {
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = reverseIt.GetIndex();
    std::cout << "Reverse iterator: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Iterator over the region forwards using a reverse iterator
  itk::ImageRegionReverseIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > backReverseIt(o3, region);
 
  backReverseIt = backReverseIt.End(); // one pixel before the region
  do
    {
    --backReverseIt;

    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = backReverseIt.GetIndex();
    std::cout << "Reverse iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;    
    }
  while (!backReverseIt.IsAtBegin()); // stop when we reach the beginning
  
  
  // Finally, create a ReverseIterator from an Iterator and walk each appropriately so that they match
  itk::ImageRegionReverseIterator<itk::Image<itk::Vector<unsigned short, 5>, 3> > castBackReverseIt(it);
  it = it.Begin();
  it.GoToBegin();
  castBackReverseIt = castBackReverseIt.End();
  int status = 0;
  std::cout << "It and Reverse check: ";
  for ( ; !it.IsAtEnd(); ++it)
    {
    --castBackReverseIt;
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType rindex = castBackReverseIt.GetIndex();    
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      if (index[i] != rindex[i])
  {
  std::cout << index[i] << " != " << rindex[i] << std::endl;
  status = 1;
  }
      }  
    }
  if (status == 0)
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    }
  return status;
}
