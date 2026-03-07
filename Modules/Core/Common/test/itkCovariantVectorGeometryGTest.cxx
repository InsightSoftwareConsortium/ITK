/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkGTest.h"
#include <iostream>

TEST(CovariantVectorGeometry, ArithmeticAndNorms)
{
  // Dimension & Type
  constexpr unsigned int N{ 3 };
  using ValueType = double;

  //  Vector type
  using VectorType = itk::CovariantVector<ValueType, N>;


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

  const VectorType vc = vb - va;
  std::cout << "vc  =  vb - va  = ";
  std::cout << vc << std::endl;

  VectorType vd = va * 5.0;
  std::cout << "vd  =  va * 5.0 = ";
  std::cout << vd << std::endl;

  VectorType ve = vd / 5.0;
  std::cout << "ve  =  vd * 5.0 = ";
  std::cout << ve << std::endl;

  vd += va;
  std::cout << "vd  +=  va      = ";
  std::cout << vd << std::endl;

  ve -= vb;
  std::cout << "ve  -=  vb      = ";
  std::cout << ve << std::endl;

  const VectorType vh = vb;
  std::cout << "vh   =  vb      = ";
  std::cout << vh << std::endl;

  VectorType vg(va);
  std::cout << "vg( va )        = ";
  std::cout << vg << std::endl;

  const ValueType norm2 = vg.GetSquaredNorm();
  std::cout << "vg squared norm = ";
  std::cout << norm2 << std::endl;
  EXPECT_DOUBLE_EQ(norm2, 54.0);

  const ValueType norm = vg.GetNorm();
  std::cout << "vg norm = ";
  std::cout << norm << std::endl;
  EXPECT_NEAR(norm, std::sqrt(54.0), 1e-10);

  const ValueType normX = vg.Normalize();
  std::cout << "vg after normalizing: " << vg << std::endl;
  EXPECT_EQ(norm, normX);


  // Test for vnl interface

  // Test the no const version that returns an vnl_vector_ref
  vnl_vector_ref<ValueType> vnlVector = va.GetVnlVector();
  {
    std::cout << "vnl_vector_ref = va ";
    for (unsigned int i = 0; i < N; ++i)
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector_ref.begin() = va.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector.begin() << " = ";
    std::cout << va.cbegin() << std::endl;
  }

  // Test the const version that returns an vnl_vector
  const VectorType      vf(va);
  vnl_vector<ValueType> vnlVector2 = vf.GetVnlVector();
  {
    std::cout << "vnl_vector = va ";
    for (unsigned int i = 0; i < N; ++i)
    {
      std::cout << vnlVector2[i] << ", ";
    }
    std::cout << std::endl;

    std::cout << "vnl_vector.begin() != vf.Begin()";
    std::cout << std::endl;
    std::cout << vnlVector2.begin() << " = ";
    std::cout << vf.cbegin() << std::endl;
  }

  // Test for CastFrom() method
  {
    std::cout << "Test for CastFrom() method... ";

    constexpr float tolerance{ 1e-7 };

    //  CovariantVector Classes
    using DoubleCovariantVectorType = itk::CovariantVector<double, N>;
    using FloatCovariantVectorType = itk::CovariantVector<float, N>;

    DoubleCovariantVectorType dp;
    dp[0] = 1.0;
    dp[1] = 1.7;
    dp[2] = 1.9;

    FloatCovariantVectorType fp;
    fp[0] = 0.0;
    fp[1] = 0.0;
    fp[2] = 0.0;

    fp.CastFrom(dp);

    std::cout << std::endl;
    for (unsigned int i = 0; i < N; ++i)
    {
      auto val = static_cast<FloatCovariantVectorType::ValueType>(dp[i]);

      const float diff = itk::Math::Absolute(val - fp[i]);
      std::cout << "difference = " << diff << std::endl;
      EXPECT_NEAR(val, fp[i], tolerance);
    }

    std::cout << " PASSED ! " << std::endl;
  }

  // Test the inner products
  {
    using ContravariantVectorType = itk::Vector<double, 3>;
    using CovariantVectorType = itk::CovariantVector<double, 3>;

    ContravariantVectorType contravariant;
    contravariant[0] = 1.0;
    contravariant[1] = 2.0;
    contravariant[2] = -7.0;

    CovariantVectorType covariant;
    covariant[0] = 1.0;
    covariant[1] = 3.0;
    covariant[2] = 5.0;

    constexpr double expectedValue{ -28.0 };

    EXPECT_TRUE(itk::Math::FloatAlmostEqual(expectedValue, covariant * contravariant));
    EXPECT_TRUE(itk::Math::FloatAlmostEqual(expectedValue, contravariant * covariant));
  }

  // Test the Cross products
  {
    using ContravariantVectorType = itk::Vector<double, 3>;
    using CovariantVectorType = itk::CovariantVector<double, 3>;

    ContravariantVectorType vaa;
    ContravariantVectorType vbb;

    vaa[0] = 1.0;
    vaa[1] = 0.0;
    vaa[2] = 0.0;

    vbb[0] = 0.0;
    vbb[1] = 1.0;
    vbb[2] = 0.0;

    CovariantVectorType normal;

    itk::CrossProduct(normal, vaa, vbb);

    CovariantVectorType expectedNormal;

    expectedNormal[0] = 0.0;
    expectedNormal[1] = 0.0;
    expectedNormal[2] = 1.0;

    EXPECT_TRUE(itk::Math::FloatAlmostEqual(normal[0], expectedNormal[0]));
    EXPECT_TRUE(itk::Math::FloatAlmostEqual(normal[1], expectedNormal[1]));
    EXPECT_TRUE(itk::Math::FloatAlmostEqual(normal[2], expectedNormal[2]));
  }
  //
  // test that the ComponentType is present
  {
    using CovariantVectorType = itk::CovariantVector<double, 3>;
    CovariantVectorType::ComponentType comp(1.0);
    double                             x(1.0);
    EXPECT_EQ(sizeof(comp), sizeof(double));
    auto * compp = reinterpret_cast<char *>(&comp);
    auto * xp = reinterpret_cast<char *>(&x);
    for (unsigned int i = 0; i < sizeof(CovariantVectorType::ComponentType); ++i)
    {
      EXPECT_EQ(compp[i], xp[i]);
    }
  }
}
