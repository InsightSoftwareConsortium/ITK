/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionTest.cxx
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

#include <iostream>
#include "itkImageRegion.h"

int itkImageRegionTest(int, char* [] )
{

  const unsigned int dimension = 3;

  typedef itk::ImageRegion< dimension >  RegionType;
  typedef RegionType::IndexType          IndexType;
  typedef RegionType::SizeType           SizeType;
  typedef RegionType::SliceRegion        SliceRegionType;

  bool passed;

  SizeType sizeA = {{ 10, 20, 30 }};
  SizeType sizeB = {{  5, 10, 15 }};

  IndexType startA = {{ 12, 12, 12 }};
  IndexType startB = {{ 14, 14, 14 }};

  RegionType regionA;
  RegionType regionB;

  regionA.SetSize(  sizeA  );
  regionA.SetIndex( startA );

  // Take slices of a region
  try
    {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(0);
    std::cout << "regionA.Slice(0): " << sliceA;
    }
  catch (itk::ExceptionObject &err)
    {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
    }

  try
    {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(1);
    std::cout << "regionA.Slice(1): " << sliceA;
    }
  catch (itk::ExceptionObject &err)
    {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
    }

  try
    {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(2);
    std::cout << "regionA.Slice(2): " << sliceA;
    }
  catch (itk::ExceptionObject &err)
    {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
    }

  try
    {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(20);
    std::cout << "regionA.Slice(20): " << sliceA;
    std::cout << "Failed to catch expected exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch (itk::ExceptionObject &err)
    {
    std::cout << "Caught expected exception" << err;
    }

  regionB.SetSize(  sizeB  );
  regionB.SetIndex( startB );

  if( regionA.IsInside( regionB ) )
    {
    passed = true;
    }
  else 
    {
    passed = false;
    }
 
  if( passed )
    {
    if( regionB.IsInside( regionA ) )
      {
      passed = false;
      }
    else 
      {
      passed = true;
      }
    }
  
  if (passed)
    {
    std::cout << "ImageRegion test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ImageRegion test failed." << std::endl;
    return EXIT_FAILURE;
    }
  
}
