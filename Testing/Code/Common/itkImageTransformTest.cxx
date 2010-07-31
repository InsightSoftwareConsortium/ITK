/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImage.h"
#include "itkImageRegion.h"
#include "itkIndex.h"
#include "itkSize.h"

#include <iostream>
#include <fstream>

template <unsigned int Dimension>
void TestTransform()
{
  typedef itk::Image<float,Dimension>         ImageType;

  typename ImageType::Pointer image =         ImageType::New();
  typename ImageType::Pointer orientedImage = ImageType::New();

  typename ImageType::PointType origin;

  for (unsigned int i=0; i < Dimension; i++)
    {
    origin[i] = static_cast<double>(i*100);
    }
  image->SetOrigin(origin);
  orientedImage->SetOrigin(origin);

  typedef itk::ImageRegion< Dimension >  RegionType;
  typedef typename RegionType::IndexType IndexType;
  typedef typename RegionType::SizeType  SizeType;

  typename ImageType::PointType point;
  RegionType region;

  SizeType size;
  size.Fill(10);
  region.SetSize( size );

  IndexType index;
  index.Fill(5);

  std::cout << "TransformIndexToPhysicalPoint..." << std::endl;
  orientedImage->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image: " << index << " -> " << point << std::endl;

  image->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image:         " << index << " -> " << point << std::endl;

  std::cout << "TransformPhysicalPointToIndex..." << std::endl;
  orientedImage->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image: " << point << " -> " << index << std::endl;

  image->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image:         " << point << " -> " << index << std::endl;
}

int itkImageTransformTest(int, char* [] ) 
{
  TestTransform<8>();
  TestTransform<3>();
  TestTransform<2>();
  TestTransform<1>();

  return EXIT_SUCCESS;

}

