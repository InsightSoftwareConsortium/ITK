/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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
