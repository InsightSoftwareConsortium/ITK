/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPixelAccessTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <class T, unsigned int VImageDimension>
void TestConstPixelAccess(const itk::Image<T, VImageDimension> &in,
                          itk::Image<T, VImageDimension> &out)
{
  itk::Image<T, VImageDimension>::Index regionStartIndex3D = {5, 10, 15};
  itk::Image<T, VImageDimension>::Index regionEndIndex3D = {8, 15, 17};

  T vec;
  unsigned short uvec[5] = {5, 4, 3, 2, 1};
  vec.GetVector().copy_in( uvec );
  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int main()
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  float origin3D[3] = { 5, 2.1, 8.1};
  float spacing3D[3] = { 1.5, 2.1, 1};

  unsigned long imageSize3D[3] = { 20, 40, 60 };
  unsigned long bufferSize3D[3] = { 8, 20, 14 };
  unsigned long regionSize3D[3] = { 4,  6,  6 };

  //  make an itk::Size class similar to itk::Index but storing unsigned longs not longs

  itk::Image<itk::Vector<unsigned short, 5>, 3>::Index startIndex3D = {-5, 4, -1};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Index bufferStartIndex3D = {2, 3, 5};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Index regionStartIndex3D = {5, 10, 12};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Index regionEndIndex3D = {8, 15, 17};


  o3->SetImageSize(imageSize3D);
  o3->SetBufferSize(bufferSize3D);
  o3->SetImageStartIndex(startIndex3D);
  o3->SetBufferStartIndex(bufferStartIndex3D);

  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;
  unsigned short uvec[5] = {5, 4, 3, 2, 1};
  vec.GetVector().copy_in(uvec);
  
  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);

  return 1;
}



