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

#include "itkMath.h"
#include "itkCovariantVector.h"
#include <iostream>

int itkCovariantVectorGeometryTest(int, char* [] )
{
  // Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

  //  Vector type
  typedef    itk::CovariantVector< ValueType, N >    VectorType;


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

  ValueType normX = vg.Normalize();
  std::cout << "vg after normalizing: " << vg << std::endl;
  if (norm != normX)
  {
      std::cout << "Norms from GetNorm() and from Normalize() are different" << std::endl;
      return EXIT_FAILURE;
  }


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

  // Test for CastFrom() method
  {
  std::cout << "Test for CastFrom() method... ";

  const float tolerance = 1e-7;

  //  CovariantVector Classes
  typedef    itk::CovariantVector<  double, N >    DoubleCovariantVectorType;
  typedef    itk::CovariantVector<  float , N >    FloatCovariantVectorType;

  DoubleCovariantVectorType dp;
  dp[0] = 1.0;
  dp[1] = 1.7;
  dp[2] = 1.9;

  FloatCovariantVectorType fp;
  fp[0] = 0.0;
  fp[1] = 0.0;
  fp[2] = 0.0;

  fp.CastFrom( dp );

  std::cout << std::endl;
  for(unsigned int i=0; i<N; i++)
    {
    FloatCovariantVectorType::ValueType val =
        static_cast< FloatCovariantVectorType::ValueType >( dp[i] );

//   std::cout << val   << std::endl;
//   std::cout << fp[i] << std::endl;

    const float diff = itk::Math::abs( val - fp[i] );
    std::cout << "difference = " << diff << std::endl;
    if( itk::Math::abs ( val - fp[i] ) > tolerance )
      {
        std::cout << "Test failed at component " << i << std::endl;
        return EXIT_FAILURE;
      }
    }

  std::cout << " PASSED ! " << std::endl;

  }

  // Test the inner products
  {
    typedef itk::Vector<double, 3>           ContravariantVectorType;
    typedef itk::CovariantVector<double, 3>  CovariantVectorType;

    ContravariantVectorType contravariant;
    contravariant[0] = 1.0;
    contravariant[1] = 2.0;
    contravariant[2] = -7.0;

    CovariantVectorType covariant;
    covariant[0] = 1.0;
    covariant[1] = 3.0;
    covariant[2] = 5.0;

    const double expectedValue = -28.0;

    if( !itk::Math::FloatAlmostEqual( expectedValue, covariant * contravariant ) ||
        !itk::Math::FloatAlmostEqual( expectedValue, contravariant * covariant ) )
        {
        std::cerr << "Error in inner product computation." << std::endl;
        return EXIT_FAILURE;
        }
  }

  // Test the Cross products
  {
    typedef itk::Vector<double, 3>           ContravariantVectorType;
    typedef itk::CovariantVector<double, 3>  CovariantVectorType;

    ContravariantVectorType vaa;
    ContravariantVectorType vbb;

    vaa[0] = 1.0;
    vaa[1] = 0.0;
    vaa[2] = 0.0;

    vbb[0] = 0.0;
    vbb[1] = 1.0;
    vbb[2] = 0.0;

    CovariantVectorType normal;

    itk::CrossProduct( normal, vaa, vbb );

    CovariantVectorType expectedNormal;

    expectedNormal[0] = 0.0;
    expectedNormal[1] = 0.0;
    expectedNormal[2] = 1.0;

    if( !itk::Math::FloatAlmostEqual( normal[0], expectedNormal[0] ) ||
        !itk::Math::FloatAlmostEqual( normal[1], expectedNormal[1] ) ||
        !itk::Math::FloatAlmostEqual( normal[2], expectedNormal[2] ) )
      {
      std::cerr << "Error in CrossProduct computation." << std::endl;
      return EXIT_FAILURE;
      }

  }
  //
  // test that the ComponentType is present
  {
  typedef itk::CovariantVector<double, 3>  CovariantVectorType;
  CovariantVectorType::ComponentType comp(1.0);
  double x(1.0);
  if(sizeof(comp) != sizeof(double))
    {
    std::cerr << "error -- CovariantVectorType::ComponentType size != sizeof(double)"
              << std::endl;
    return EXIT_FAILURE;
    }
  char *compp = reinterpret_cast<char *>(&comp);
  char *xp = reinterpret_cast<char *>(&x);
  for(unsigned i = 0; i < sizeof(CovariantVectorType::ComponentType); i++)
    {
    if(compp[i] != xp[i])
      {
      std::cerr << "error -- bit pattern for CovariantVectorType::ComponentType doesn't match "
                << " double with same value" << std::endl;
      }
    }
  }
  return EXIT_SUCCESS;
}
