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
#include "itkGeometryUtilities.h"
#include "itkMath.h"

namespace itk
{

long
GeometryUtilities
::Factorial(const long n)
{
  if ( n < 1 )
    {
    return 1;
    }
  return n * Factorial(n - 1);
}

long
GeometryUtilities
::DoubleFactorial(const long n)
{
  if ( n < 2 )
    {
    return 1;
    }
  return n * DoubleFactorial(n - 2);
}

double
GeometryUtilities
::GammaN2p1(const long n)
{
  const bool even = n % 2 == 0;

  if ( even )
    {
    return Factorial(n / 2);
    }
  else
    {
    return std::sqrt(itk::Math::pi) * DoubleFactorial(n) / std::pow(2, ( n + 1 ) / 2.0);
    }
}

double
GeometryUtilities
::HyperSphereVolume(const int dim, const double radius)
{
  const double dbldim = static_cast< double >( dim );

  return std::pow(itk::Math::pi, dbldim * 0.5) * std::pow(radius, dbldim) / GammaN2p1(dim);
}

double
GeometryUtilities
::HyperSpherePerimeter(const int dim, const double radius)
{
  return dim * HyperSphereVolume(dim, radius) / radius;
}

double
GeometryUtilities
::HyperSphereRadiusFromVolume(const int dim, const double volume)
{
  return std::pow(volume * GammaN2p1(dim) / std::pow(itk::Math::pi, dim * 0.5), 1.0 / dim);
}

} // end of itk namespace
