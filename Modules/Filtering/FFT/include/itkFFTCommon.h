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
#ifndef itkFFTCommon_h
#define itkFFTCommon_h

#include "itkIntTypes.h"
#include <vnl/vnl_math.h>

namespace itk
{

inline bool IsPrime( SizeValueType n )
{
  if( n <= 1 )
  {
    return false;
  }
  const SizeValueType last = (SizeValueType)vcl_sqrt( (double)n );
  for( SizeValueType x=2; x<=last; ++x )
    {
    if( n%x == 0 )
      {
      return false;
      }
    }
  return true;
}

inline SizeValueType GreatestPrimeFactor( SizeValueType n )
{
  SizeValueType v = 2;
  while( v <= n )
    {
    if( n % v == 0 && IsPrime( v ) )
      {
      n /= v;
      }
    else
      {
      v += 1;
      }
    }
  return v;
}

}
#endif
