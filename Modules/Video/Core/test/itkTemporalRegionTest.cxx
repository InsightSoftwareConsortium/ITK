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
#include "itkTemporalRegion.h"

/**
 * Test the basic functionality of Temporal Regions
 */
int itkTemporalRegionTest( int , char* [] )
{

#define CHECK_FOR_VALUE(a,b)                                            \
    {                                                                     \
    if( a != b )                                                        \
      {                                                                 \
      std::cerr << "Error in " #a << " expected " << b << " but got " << a << std::endl; \
      return EXIT_FAILURE;                                              \
      }                                                                 \
    }

  // Test arrays for frame durations
  itk::SizeValueType testFrameStart = 0;
  itk::SizeValueType   testFrameDuration = 20;

  // Test time stamps and intervals
  itk::RealTimeStamp    stamp0;
  itk::RealTimeStamp    stamp1 = stamp0;
  itk::RealTimeInterval oneSecond( 1, 0 );
  itk::RealTimeInterval tenSeconds( 10, 0 );
  for( unsigned int i=0; i < 1000000L; i++)
    {
    stamp1 += oneSecond;
    }

  // Create TemporalRegions
  itk::TemporalRegion regionA;
  itk::TemporalRegion regionB;

  // Check value of regions at init
  bool shouldBeTrue = regionA == regionB;
  CHECK_FOR_VALUE(shouldBeTrue,true);

  bool shouldBeFalse = regionA != regionB;
  CHECK_FOR_VALUE(shouldBeFalse,false);

  // Set new start times
  regionA.SetRealStart(stamp0);
  regionB.SetRealStart(stamp1);

  // Check values of set start times
  CHECK_FOR_VALUE(regionA.GetRealStart(),stamp0);
  CHECK_FOR_VALUE(regionB.GetRealStart(),stamp1);

  // Set toy durations
  regionA.SetRealDuration(tenSeconds);
  regionB.SetRealDuration(oneSecond);

  // Check values of set durations
  CHECK_FOR_VALUE(regionA.GetRealDuration(),tenSeconds);
  CHECK_FOR_VALUE(regionB.GetRealDuration(),oneSecond);

  // Check set/get frame start
  regionA.SetFrameStart( testFrameStart );
  CHECK_FOR_VALUE(regionA.GetFrameStart(), testFrameStart);

  // Check set frame duration
  regionB.SetFrameDuration( testFrameDuration );
  CHECK_FOR_VALUE(regionB.GetFrameDuration(), testFrameDuration);

  // Check the region type
  CHECK_FOR_VALUE(regionA.GetRegionType(),itk::TemporalRegion::ITK_STRUCTURED_REGION);

  return EXIT_SUCCESS;
}
