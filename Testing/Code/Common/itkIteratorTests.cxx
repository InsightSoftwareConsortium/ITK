/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkIteratorTests.cxx
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
#include "itkImageRegionIterator.h"
#include "itkImageBufferIterator.h"
#include "itkImageRegionSimpleIterator.h"
#include <time.h>

int main()
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Scalar<unsigned short>, 3>::Pointer
    o3 = itk::Image<itk::Scalar<unsigned short>, 3>::New();

  float origin3D[3] = { 5, 2.1, 8.1};
  float spacing3D[3] = { 1.5, 2.1, 1};

  itk::Image<itk::Scalar<unsigned short>, 3>::Size imageSize3D = { 200, 200, 200 };
  itk::Image<itk::Scalar<unsigned short>, 3>::Size bufferSize3D = { 200, 200, 200 };
  itk::Image<itk::Scalar<unsigned short>, 3>::Size regionSize3D = { 190, 190, 190 };

  itk::Image<itk::Scalar<unsigned short>, 3>::Index startIndex3D = {0, 0, 0};
  itk::Image<itk::Scalar<unsigned short>, 3>::Index bufferStartIndex3D = {0, 0, 0};
  itk::Image<itk::Scalar<unsigned short>, 3>::Index regionStartIndex3D = {5,5, 5};
  itk::Image<itk::Scalar<unsigned short>, 3>::Index regionEndIndex3D = {194, 194, 194};


  itk::Image<itk::Scalar<unsigned short>, 3>::Region region;
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

  // extra variables
  double elapsedTime;
  clock_t start, end;
  unsigned long num = 190*190*190;
  unsigned long i = 0;

  // memset
  start = clock();
  itk::Scalar<unsigned short> *ptr = o3->GetBufferPointer();
  memset(ptr, 0, num*sizeof(unsigned short));
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer using memset" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << num << std::endl;

  // 1D array
  start = clock();
  i = 0;
  ptr = o3->GetBufferPointer();
  for (i=0; i < num; ++i)
    {
    *ptr = 5;
    ++ptr;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer as a 1D array" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;


  // 3 nested loops
  unsigned long ii, jj, kk, len=190;
  start = clock();
  i = 0;
  ptr = o3->GetBufferPointer();
  for (ii=0; ii < len; ++ii)
    for (jj=0; jj < len; ++jj)
      for (kk=0; kk < len; ++kk)
        {
        *ptr = 5;
        ++ptr;
        ++i;
        }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "Raw pointer in 3 nested loops" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;
  
  // ImageRegionIterator
  start = clock();
  itk::ImageRegionIterator<itk::Scalar<unsigned short>, 3> it(o3, region);

  i = 0;
  for ( ; !it.IsAtEnd(); ++it)
    {
    *it = 5;
    ++i;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "ImageRegionIterator" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;

  // ImageRegionSimpleIterator
  start = clock();
  itk::ImageRegionSimpleIterator<itk::Image<itk::Scalar<unsigned short>, 3> > it2(o3, region);

  i = 0;
  for ( it2.Begin(); !it2.IsAtEnd(); ++it2)
    {
    *it2 = 5;
    ++i;
    }
  end = clock();
  elapsedTime = (end - start) / (double) CLOCKS_PER_SEC;

  std::cout << "ImageRegionSimpleIterator" << std::endl;
  std::cout << "\tTime   = " << elapsedTime << std::endl;
  std::cout << "\tPixels = " << i << std::endl;
  
  return 1;
}



