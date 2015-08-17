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

#include <iostream>
#include "itkRealTimeStamp.h"
#include "itkNumericTraits.h"

#define CHECK_FOR_VALUE(a,b)                                            \
  {                                                                     \
    double eps = 4.0*itk::NumericTraits<double>::epsilon();             \
CLANG_PRAGMA_PUSH                                                       \
CLANG_SUPPRESS_Wfloat_equal                                              \
    eps = ( b == 0.0 ) ? eps : std::fabs( b*eps );                       \
CLANG_PRAGMA_POP                                                        \
    if( std::fabs( a - b ) >  eps)                                       \
      {                                                                 \
      std::cerr << "Error in "#a << " expected " << b << " but got " << a << std::endl; \
      return EXIT_FAILURE;                                              \
      }                                                                 \
  }

#define CHECK_FOR_BOOLEAN( x, expected ) \
  { \
  if( (x) != expected ) \
    { \
    std::cerr << "Error in "#x << std::endl; \
    return EXIT_FAILURE; \
    } \
  }


int itkRealTimeStampTest( int, char * [] )
{
  itk::RealTimeStamp stamp0;

  double timeInMicroSeconds = stamp0.GetTimeInMicroSeconds();
  double timeInMilliSeconds = stamp0.GetTimeInMilliSeconds();
  double timeInSeconds      = stamp0.GetTimeInSeconds();
  double timeInHours        = stamp0.GetTimeInHours();
  double timeInDays         = stamp0.GetTimeInDays();

  CHECK_FOR_VALUE( timeInMicroSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInMilliSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInHours, 0.0 );
  CHECK_FOR_VALUE( timeInDays, 0.0 );

  itk::RealTimeStamp stamp1;
  itk::RealTimeStamp stamp2 = stamp0;
  itk::RealTimeInterval oneSecond( 1, 0 );

  for( unsigned int i=0; i < 1000000L; i++)
    {
    stamp2 += oneSecond;
    }

  std::cout << "Stamp2 = " << stamp2 << std::endl;

  itk::RealTimeInterval manySeconds = stamp2 - stamp0;

  timeInSeconds = manySeconds.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 1000000.0 );

  itk::RealTimeInterval fiveMicroseconds;
  fiveMicroseconds.Set( 0, 5 );

  itk::RealTimeStamp stamp3 = stamp0;

  for( unsigned int i=0; i < 1000000L; i++)
    {
    stamp3 += fiveMicroseconds;
    }

  manySeconds = stamp3 - stamp0;

  timeInSeconds = manySeconds.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 5.0 );

  for( unsigned int i=0; i < 1000000L; i++)
    {
    stamp3 -= fiveMicroseconds;
    }

  manySeconds = stamp3 - stamp0;

  timeInSeconds = manySeconds.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 0.0 );


  itk::RealTimeInterval timeSpan;

  timeSpan.Set( 19, -5000000L );

  timeInSeconds = timeSpan.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 14.0 );

  timeSpan.Set( -19, 5000000L );

  timeInSeconds = timeSpan.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, -14.0 );

  timeSpan.Set( -19, -5000000L );

  timeInSeconds = timeSpan.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, -24.0 );

  timeSpan.Set( 19, 5000000L );

  timeInSeconds = timeSpan.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 24.0 );


  itk::RealTimeInterval timeSpan1( 19, 300000L );
  itk::RealTimeInterval timeSpan2( 13, 500000L );

  itk::RealTimeInterval timeSpan3 = timeSpan1 + timeSpan2;

  timeInSeconds = timeSpan3.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 32.8 );

  // Test comparison operations
  itk::RealTimeInterval dt1( 15, 13 );
  itk::RealTimeInterval dt2( 19, 11 );
  itk::RealTimeInterval dt3( 15, 25 );

  itk::RealTimeInterval t1;
  t1 += dt1;

  itk::RealTimeInterval t2;
  t2 += dt2;

  itk::RealTimeInterval t3;
  t3 += dt3;

  CHECK_FOR_BOOLEAN(  t1 == t1, true  );
  CHECK_FOR_BOOLEAN(  t1 != t2, true  );
  CHECK_FOR_BOOLEAN(  t1 != t1, false );
  CHECK_FOR_BOOLEAN(  t2 >= t1, true  );
  CHECK_FOR_BOOLEAN(  t1 >= t1, true  );
  CHECK_FOR_BOOLEAN(  t2 >  t1, true  );
  CHECK_FOR_BOOLEAN(  t1 <= t2, true  );
  CHECK_FOR_BOOLEAN(  t1 <= t1, true  );
  CHECK_FOR_BOOLEAN(  t1 <  t2, true  );

  CHECK_FOR_BOOLEAN(  t3 == t3, true  );
  CHECK_FOR_BOOLEAN(  t1 != t3, true  );
  CHECK_FOR_BOOLEAN(  t3 >= t1, true  );
  CHECK_FOR_BOOLEAN(  t3 >  t1, true  );
  CHECK_FOR_BOOLEAN(  t3 <= t1, false );
  CHECK_FOR_BOOLEAN(  t3 <  t1, false );
  CHECK_FOR_BOOLEAN(  t1 <= t3, true  );
  CHECK_FOR_BOOLEAN(  t1 <  t3, true  );
  CHECK_FOR_BOOLEAN(  t1 >= t3, false );
  CHECK_FOR_BOOLEAN(  t1 >  t3, false );


  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
