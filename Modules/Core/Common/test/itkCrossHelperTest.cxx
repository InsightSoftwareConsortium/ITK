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
#include "itkVector.h"
#include "itkCrossHelper.h"

#include <iostream>


int itkCrossHelperTest( int argc, char* argv[] )
{
  (void) argc;
  (void) argv;

  const unsigned int Dimension2D = 2;
  const unsigned int Dimension3D = 3;
  const unsigned int Dimension4D = 4;

  typedef double CoordRepType;

  typedef itk::Vector< CoordRepType, Dimension2D > Vector2DType;
  typedef itk::Vector< CoordRepType, Dimension3D > Vector3DType;
  typedef itk::Vector< CoordRepType, Dimension4D > Vector4DType;

  typedef itk::CrossHelper< Vector2DType > Cross2DType;
  Cross2DType cross2d;

  typedef itk::CrossHelper< Vector3DType > Cross3DType;
  Cross3DType cross3d;

  typedef itk::CrossHelper< Vector4DType > Cross4DType;
  Cross4DType cross4d;

  Vector2DType u2d;
  u2d[0] = 1.;
  u2d[1] = 0.;

  Vector2DType v2d;
  v2d[0] = 0.;
  v2d[1] = 1.;

  if( cross2d( u2d, v2d ).GetNorm( ) > 1e-6 )
    {
    std::cout <<"cross product must return null vector is dimension is below 3"
       <<std::endl;
    return EXIT_FAILURE;
    }

  Vector3DType u3d;
  u3d[0] = 1.;
  u3d[1] = 0.;
  u3d[2] = 0.;

  Vector3DType v3d;
  v3d[0] = 0.;
  v3d[1] = 1.;
  v3d[2] = 0.;

  Vector3DType w3d;
  w3d[0] = 0.;
  w3d[1] = 0.;
  w3d[2] = 1.;

  if( ( cross3d( u3d, v3d ) - w3d ).GetNorm() > 1e-6 )
    {
    std::cout <<"cross3d( u3d, v3d ) != w3d" <<std::endl;
    return EXIT_FAILURE;
    }

  if( ( cross3d( v3d, w3d ) - u3d ).GetNorm() > 1e-6 )
    {
    std::cout <<"cross3d( v3d, w3d ) != u3d" <<std::endl;
    return EXIT_FAILURE;
    }

  if( ( cross3d( w3d, u3d ) - v3d ).GetNorm() > 1e-6 )
    {
    std::cout <<"cross3d( w3d, u3d ) != v3d" <<std::endl;
    return EXIT_FAILURE;
    }

  Vector4DType u4d;
  u4d[0] = 1.;
  u4d[1] = 0.;
  u4d[2] = 0.;
  u4d[3] = 0.;

  Vector4DType v4d;
  v4d[0] = 0.;
  v4d[1] = 1.;
  v4d[2] = 0.;
  v4d[3] = 0.;

  Vector4DType w4d;
  w4d[0] = 0.;
  w4d[1] = 0.;
  w4d[2] = 1.;
  w4d[3] = 0.;

  if( ( cross4d( u4d, v4d ) - w4d ).GetNorm() > 1e-6 )
    {
    std::cout <<"cross4d( u4d, v4d ) != w4d" <<std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
