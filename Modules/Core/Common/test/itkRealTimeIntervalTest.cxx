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
#include "itkRealTimeInterval.h"
#include "itkMacro.h"
#include "itkNumericTraits.h"

#define CHECK_FOR_VALUE(a,b) \
  { \
    double eps = 4.0*itk::NumericTraits<double>::epsilon();             \
CLANG_PRAGMA_PUSH                                                       \
CLANG_SUPPRESS_Wfloat_equal                                             \
    eps = ( b == 0.0 ) ? eps : std::fabs(b*eps);                         \
CLANG_PRAGMA_POP                                                         \
    if( std::fabs( a - b ) > eps )                                       \
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


int itkRealTimeIntervalTest( int, char * [] )
{
  itk::RealTimeInterval interval0;

  double timeInMicroSeconds = interval0.GetTimeInMicroSeconds();
  double timeInMilliSeconds = interval0.GetTimeInMilliSeconds();
  double timeInSeconds      = interval0.GetTimeInSeconds();
  double timeInHours        = interval0.GetTimeInHours();
  double timeInDays         = interval0.GetTimeInDays();

  CHECK_FOR_VALUE( timeInMicroSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInMilliSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInSeconds, 0.0 );
  CHECK_FOR_VALUE( timeInHours, 0.0 );
  CHECK_FOR_VALUE( timeInDays, 0.0 );

  itk::RealTimeInterval interval1;
  itk::RealTimeInterval intervalX = interval0;

  itk::RealTimeInterval oneSecond( 1, 0 );
  for( unsigned int i=0; i < 1000000L; i++)
    {
    intervalX += oneSecond;
    }

  std::cout << "intervalX = " << intervalX << std::endl;

  itk::RealTimeInterval manySeconds = intervalX - interval0;

  timeInSeconds = manySeconds.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 1000000.0 );

  itk::RealTimeInterval fiveMicroseconds;
  fiveMicroseconds.Set( 0, 5 );

  itk::RealTimeInterval interval3 = interval0;

  for( unsigned int i=0; i < 1000000L; i++)
    {
    interval3 += fiveMicroseconds;
    }

  manySeconds = interval3 - interval0;

  timeInSeconds = manySeconds.GetTimeInSeconds();

  CHECK_FOR_VALUE( timeInSeconds, 5.0 );

  for( unsigned int i=0; i < 1000000L; i++)
    {
    interval3 -= fiveMicroseconds;
    }

  manySeconds = interval3 - interval0;

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

  CHECK_FOR_BOOLEAN(  dt1 == dt1, true  );
  CHECK_FOR_BOOLEAN(  dt1 != dt2, true  );
  CHECK_FOR_BOOLEAN(  dt1 != dt1, false );
  CHECK_FOR_BOOLEAN(  dt2 >= dt1, true  );
  CHECK_FOR_BOOLEAN(  dt1 >= dt1, true  );
  CHECK_FOR_BOOLEAN(  dt2 >  dt1, true  );
  CHECK_FOR_BOOLEAN(  dt1 <= dt2, true  );
  CHECK_FOR_BOOLEAN(  dt1 <= dt1, true  );
  CHECK_FOR_BOOLEAN(  dt1 <  dt2, true  );

  CHECK_FOR_BOOLEAN(  dt3 == dt3, true  );
  CHECK_FOR_BOOLEAN(  dt1 != dt3, true  );
  CHECK_FOR_BOOLEAN(  dt3 >= dt1, true  );
  CHECK_FOR_BOOLEAN(  dt3 >  dt1, true  );
  CHECK_FOR_BOOLEAN(  dt3 <= dt1, false );
  CHECK_FOR_BOOLEAN(  dt3 <  dt1, false );
  CHECK_FOR_BOOLEAN(  dt1 <= dt3, true  );
  CHECK_FOR_BOOLEAN(  dt1 <  dt3, true  );
  CHECK_FOR_BOOLEAN(  dt1 >= dt3, false );
  CHECK_FOR_BOOLEAN(  dt1 >  dt3, false );


  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
