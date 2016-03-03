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

#include "itkImageRegionSplitterMultidimensional.h"
#include "itkImageRegion.h"
#include "itkTestingMacros.h"
#include <iostream>

int itkImageRegionSplitterMultidimensionalTest(int, char*[])
{

  itk::ImageRegionSplitterMultidimensional::Pointer splitter =
    itk::ImageRegionSplitterMultidimensional::New();

  EXERCISE_BASIC_OBJECT_METHODS( splitter,
    ImageRegionSplitterMultidimensional, ImageRegionSplitterBase );

  itk::ImageRegion<2> region;
  region.SetSize(0, 10);
  region.SetSize(1, 11);

  region.SetIndex(0, 1);
  region.SetIndex(1, 10);

  const itk::ImageRegion<2> lpRegion = region;

  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 1 ), 1 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 2 ), 2 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 3 ), 2 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 4 ), 4 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 7 ), 6 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 11 ), 9 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 12 ), 12 );
  TEST_EXPECT_EQUAL( splitter->GetNumberOfSplits( lpRegion, 99 ), 90 );


  region = lpRegion;
  splitter->GetSplit(0, 2, region);
  TEST_EXPECT_EQUAL(region.GetSize(0), 10);
  TEST_EXPECT_EQUAL(region.GetSize(1), 5);

  region = lpRegion;
  splitter->GetSplit(1, 2, region);
  TEST_EXPECT_EQUAL(region.GetSize(0), 10);
  TEST_EXPECT_EQUAL(region.GetSize(1), 6);


  region = lpRegion;
  splitter->GetSplit(0, 4, region);
  TEST_EXPECT_EQUAL(region.GetIndex(0), 1);
  TEST_EXPECT_EQUAL(region.GetIndex(1), 10);
  TEST_EXPECT_EQUAL(region.GetSize(0), 5);
  TEST_EXPECT_EQUAL(region.GetSize(1), 5);

  region = lpRegion;
  splitter->GetSplit(3, 4, region);
  TEST_EXPECT_EQUAL(region.GetIndex(0), 6);
  TEST_EXPECT_EQUAL(region.GetIndex(1), 15);
  TEST_EXPECT_EQUAL(region.GetSize(0), 5);
  TEST_EXPECT_EQUAL(region.GetSize(1), 6);

  return EXIT_SUCCESS;
}
