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

/**
 *
 *  This program illustrates the use of Geometric objects
 *
 */


#include "itkVector.h"
#include <iostream>

//-------------------------
//
//   Main code
//
//-------------------------
int itkVectorGeometryTest(int, char* [] )
{

  // Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

  //  Vector type
  typedef    itk::Vector< ValueType, N >    VectorType;

/*
  VectorType vv;
  vv = 0, 2, 4;

  if( vv[0] != 0 || vv[1] != 2 || vv[2] != 4 )
    {
    std::cerr << "Error initializing the Vector " << std::endl;
    return EXIT_FAILURE;
    }
  */

  VectorType va;
  va[0] = 1.0;
  va[1] = 2.0;
  va[2] = 7.0;

  std::cout << "va = { 1.0, 2.0, 7.0 } = ";
  std::cout << va << std::endl;

  VectorType vb;

  vb[0] = 1.0;
  vb[1] = 3.0;
  vb[2] = 5.0;

  std::cout << "vb = (1,3,5)   = ";
  std::cout << vb << std::endl;

  VectorType   vc  =  vb - va;
  std::cout << "vc  =  vb - va  = ";
  std::cout << vc << std::endl;

  VectorType   vd  =  va * 5.0;
  std::cout << "vd  =  va * 5.0 = ";
  std::cout << vd << std::endl;

  VectorType   ve  =  vd / 5.0;
  std::cout << "ve  =  vd * 5.0 = ";
  std::cout << ve << std::endl;

  vd += va;
  std::cout << "vd  +=  va      = ";
  std::cout << vd << std::endl;

  ve -= vb;
  std::cout << "ve  -=  vb      = ";
  std::cout << ve << std::endl;

  VectorType   vh  =  vb;
  std::cout << "vh   =  vb      = ";
  std::cout << vh << std::endl;


  VectorType   vg( va );
  std::cout << "vg( va )        = ";
  std::cout << vg << std::endl;


  ValueType norm2 = vg.GetSquaredNorm();
  std::cout << "vg squared norm = ";
  std::cout << norm2 << std::endl;

  ValueType norm  = vg.GetNorm();
  std::cout << "vg norm = ";
  std::cout << norm << std::endl;


  // Test for vnl interface

  // Test the no const version that returns an vnl_vector_ref
  vnl_vector_ref< ValueType > vnlVector = va.GetVnlVector();
  {
    std::cout << "vnl_vector_ref = va ";
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector_ref.begin() = va.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector.begin() << " = ";
    std::cout << va.Begin() << std::endl;
  }

  // Test the const version that returns an vnl_vector
  const VectorType vf(va);
  vnl_vector<ValueType> vnlVector2 = vf.GetVnlVector();
  {
    std::cout << "vnl_vector = va ";
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector2[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector.begin() != vf.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector2.begin() << " = ";
    std::cout << vf.Begin() << std::endl;
  }


  // Test for operator== and operator!=
  {
    VectorType vv;
    vv[0] = 1;
    vv[1] = 3;
    vv[2] = 5;

    VectorType vw;
    vw.Fill( 0 );

    if( vv == vw )
      {
      std::cout << std::endl;
      std::cout << "Problem with operator==() " << std::endl;
      std::cout << "Vector " << vv;
      std::cout << " is reported as being equal to " << std::endl;
      std::cout << "Vector " << vw << std::endl;
      return EXIT_FAILURE;
      }

    VectorType ww;
    ww = vv;

    if( vv != ww )
    {
      std::cout << std::endl;
      std::cout << "Problem with operator!=() " << std::endl;
      std::cout << "Vector " << vv;
      std::cout << " is reported as being different from " << std::endl;
      std::cout << "Vector " << ww << std::endl;
      return EXIT_FAILURE;
    }

    if( !( vv == ww ) )
    {
      std::cout << std::endl;
      std::cout << "Problem with operator==() " << std::endl;
      std::cout << "Vector " << vv;
      std::cout << " is reported as not being equal to " << std::endl;
      std::cout << "Vector " << ww << std::endl;
      return EXIT_FAILURE;
    }

  }

  // Test for CastFrom() method
  {
  std::cout << "Test for CastFrom() method... ";

  const float tolerance = 1e-7;

  //  Vector Classes
  typedef    itk::Vector<  double, N >    DoubleVectorType;
  typedef    itk::Vector<  float , N >    FloatVectorType;

  DoubleVectorType dp;
  dp[0] = 1.0;
  dp[1] = 1.7;
  dp[2] = 1.9;

  FloatVectorType fp;
  fp[0] = 0.0;
  fp[1] = 0.0;
  fp[2] = 0.0;


  fp.CastFrom( dp );


  for(unsigned int i=0; i<N; i++)
    {
    FloatVectorType::ValueType val =
        static_cast< FloatVectorType::ValueType >( dp[i] );
    if( itk::Math::abs( val - fp[i] ) > tolerance )
      {
        std::cout << "Test failed at component " << i << std::endl;
        return EXIT_FAILURE;
      }
    }


  std::cout << " PASSED ! " << std::endl;

  }
  return EXIT_SUCCESS;

}
