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
#include "itkMath.h"

namespace itk
{
namespace Math
{

namespace
{

template <typename T>
inline bool
IsPrime( T n )
{
  if( n <= 1 )
  {
    return false;
  }
  const T last = (T)std::sqrt( (double)n );
  for( T x=2; x<=last; ++x )
    {
    if( n%x == 0 )
      {
      return false;
      }
    }
  return true;
}

template <typename T>
inline T
GreatestPrimeFactor( T n )
{
  T v = 2;
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

} // end anonymous namespace


bool IsPrime( unsigned short n )
{
  return IsPrime<unsigned short>(n);
}

bool IsPrime( unsigned int n )
{
  return IsPrime<unsigned int>(n);
}

bool IsPrime( unsigned long n )
{
  return IsPrime<unsigned long>(n);
}

bool IsPrime( unsigned long long n )
{
  return IsPrime<unsigned long long>(n);
}


unsigned short GreatestPrimeFactor( unsigned short n )
{
  return GreatestPrimeFactor<unsigned short>(n);
}

unsigned int GreatestPrimeFactor( unsigned int n )
{
  return GreatestPrimeFactor<unsigned int>(n);
}

unsigned long GreatestPrimeFactor( unsigned long n )
{
  return GreatestPrimeFactor<unsigned long>(n);
}

unsigned long long GreatestPrimeFactor( unsigned long long n )
{
  return GreatestPrimeFactor<unsigned long long>(n);
}

} // end namespace Math
} // end namespace itk
