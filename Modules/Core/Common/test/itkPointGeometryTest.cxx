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
#include "itkPoint.h"
#include "itkVectorContainer.h"
#include <iostream>

//-------------------------
//
//   Main code
//
//-------------------------
int itkPointGeometryTest(int, char* [] )
{

// Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

//  Vector & Point Classes
  typedef    itk::Vector< ValueType, N >    VectorType;
  typedef    itk::Point<  ValueType, N >    PointType;

  VectorType va;

  va[0] = 1.0;
  va[1] = 3.0;
  va[2] = 5.0;

  std::cout << "initial vector va = ";
  std::cout << va << std::endl;

  // Tests for Point Type

  PointType   pa;
  pa[0] =  1.0;
  pa[1] =  5.0;
  pa[2] = 11.0;

  std::cout << "initial point pa = ";
  std::cout << pa << std::endl;

  PointType   pb( pa );
  std::cout << "copy constructor pb(pa) = ";
  std::cout << pb << std::endl;

  PointType   pc = pa;
  std::cout << "copy constructor pc=pa  = ";
  std::cout << pc << std::endl;

  PointType   pd = pa + va;
  std::cout << "vector sum pd = pa + va = ";
  std::cout << pd << std::endl;

  pb = pd + va;
  std::cout << "vector sum pb = pd + va = ";
  std::cout << pb << std::endl;

  VectorType  diff = pa - pb;
  std::cout << "diff = pa - pb = ";
  std::cout << diff << std::endl;

  pc -= va;
  std::cout << "pc -= va = ";
  std::cout << pc << std::endl;

  pc += va;
  std::cout << "pc += va = ";
  std::cout << pc << std::endl;

  ValueType distance = pc.EuclideanDistanceTo( pb );
  std::cout << "Euclidean distance between pc and pb = ";
  std::cout << distance << std::endl;

  ValueType distance2 = pc.SquaredEuclideanDistanceTo( pb );
  std::cout << "Squared Euclidean distance between pc and pb = ";
  std::cout << distance2 << std::endl;

  vnl_vector_ref< ValueType > vnlVector = pa.GetVnlVector();
  std::cout << "vnl_vector = ";
  {
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;
  }


  // Test for CastFrom() method
  {
  std::cout << "Test for CastFrom() method... ";

  const float tolerance = 1e-7;

  //  Point Classes
  typedef    itk::Point<  double, N >    DoublePointType;
  typedef    itk::Point<  float , N >    FloatPointType;

  DoublePointType dp;
  dp[0] = 1.0;
  dp[1] = 1.7;
  dp[2] = 1.9;

  FloatPointType fp;
  fp[0] = 0.0;
  fp[1] = 0.0;
  fp[2] = 0.0;


  fp.CastFrom( dp );  // Here is the call !

  // Verification...
  for(unsigned int i=0; i<N; i++)
    {
    FloatPointType::ValueType val =
        static_cast< FloatPointType::ValueType >( dp[i] );
    if( std::fabs ( val - fp[i] ) > tolerance )
      {
        std::cout << "Failed at component " << i << std::endl;
        return EXIT_FAILURE;
      }
    }


  std::cout << " PASSED ! " << std::endl;

  }

  // Test the MeanPoint
  {
    PointType midpoint;
    PointType::ValueType aInit[3] = {2.0,4.0,7.0};
    PointType::ValueType bInit[3] = {6.0,2.0,9.0};
    PointType A = aInit;
    PointType B = bInit;
    midpoint.SetToMidPoint( A, B );
    std::cout << "Test for MidPoint " << std::endl;
    std::cout << "PA = " << A << std::endl;
    std::cout << "PB = " << B << std::endl;
    std::cout << "MidPoint = " << midpoint << std::endl;
    for(unsigned int i=0; i<N; i++ )
    {
      if( itk::Math::NotAlmostEquals( midpoint[i], (A[i]+B[i])/2.0 ) )
      {
        std::cerr << "Failure to compute MidPoint " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Test for MidPoint point PASSED" << std::endl;
  }


  // Test the Barycentric combination
  {
    const double tolerance = 1e-10;
    PointType combination;
    PointType::ValueType aInit[3] = {2.0,4.0,7.0};
    PointType::ValueType bInit[3] = {6.0,2.0,9.0};
    PointType A = aInit;
    PointType B = bInit;
    double alpha = 0.5;
    combination.SetToBarycentricCombination( A, B, alpha );
    std::cout << "Test for Barycentric combination" << std::endl;
    std::cout << "PA = " << A << std::endl;
    std::cout << "PB = " << B << std::endl;
    std::cout << "Alpha = " << alpha << std::endl;
    std::cout << "Combination = " << combination << std::endl;
    for(unsigned int i=0; i<N; i++ )
    {
      const double value = (alpha*A[i]+(1.0-alpha)*B[i]);
      if( std::fabs(combination[i] - value ) > tolerance )
      {
        std::cerr << "Failure to compute Barycentric combination" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Test for Barycentric combination PASSED" << std::endl;
  }

  // Test the Barycentric combination
  {
    const double tolerance = 1e-10;
    PointType combination;
    PointType::ValueType aInit[3] = {12.0,  0.0,  0.0};
    PointType::ValueType bInit[3] = { 0.0,  0.0, 12.0};
    PointType::ValueType cInit[3] = { 0.0, 12.0,  0.0};
    PointType A = aInit;
    PointType B = bInit;
    PointType C = cInit;
    double alpha = 1.0/3.0;
    double beta  = 1.0/3.0;
    combination.SetToBarycentricCombination( A, B, C, alpha, beta );
    std::cout << "Test for Barycentric combination" << std::endl;
    std::cout << "PA = " << A << std::endl;
    std::cout << "PB = " << B << std::endl;
    std::cout << "PC = " << C << std::endl;
    std::cout << "Alpha = " << alpha << std::endl;
    std::cout << "Beta  = " << beta  << std::endl;
    std::cout << "Combination = " << combination << std::endl;
    for(unsigned int i=0; i<N; i++ )
    {
      const double value = alpha*A[i]+beta*B[i]+(1.0-alpha-beta)*C[i];
      if( std::fabs( combination[i] - value ) > tolerance )
      {
        std::cerr << "Failure to compute Barycentric combination" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Test for Barycentric combination PASSED" << std::endl;
  }

 // Test the Barycentric combination for an array
  {
    const double tolerance = 1e-10;
    PointType combination;
    const unsigned int NP = 3;
    PointType A[NP];
    double     w[NP-1];
    const double K = 12.0;
    PointType::ValueType aInit0[3] = {   K,  0.0, 0.0};
    PointType::ValueType aInit1[3] = { 0.0,    K, 0.0};
    PointType::ValueType aInit2[3] = { 0.0,  0.0,   K};
    A[0] =  aInit0;
    A[1] =  aInit1;
    A[2] =  aInit2;
    w[0] = 1/3.0;
    w[1] = 1/3.0;
    combination.SetToBarycentricCombination( A, w, N );
    std::cout << "Test for Barycentric combination of an array of Points" << std::endl;
    for(unsigned int i=0; i<N; i++ )
    {
      if( std::fabs( combination[i] - (K/3.0) ) > tolerance )
      {
        std::cerr << "Failure to compute Barycentric combination" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Test for Barycentric combination of an array of Points PASSED" << std::endl;
  }


 // Test the Barycentric combination for an VectorContainer of Points
  {
    const double tolerance = 1e-10;
    PointType combination;
    const unsigned int NP = 3;
    typedef itk::VectorContainer<unsigned long,PointType>  VectorOfPoints;
    VectorOfPoints::Pointer points = VectorOfPoints::New();
    points->Reserve(NP);
    const double K = 12.0;

    VectorOfPoints::Iterator point = points->Begin();
    PointType::ValueType vInit0[3] = {   K,  0.0, 0.0};
    PointType::ValueType vInit1[3] = { 0.0,    K, 0.0};
    PointType::ValueType vInit2[3] = { 0.0,  0.0,   K};
    point->Value() =  vInit0;
    point++;
    point->Value() =  vInit1;
    point++;
    point->Value() =  vInit2;

    double     w[NP-1];
    w[0] = 1/3.0;
    w[1] = 1/3.0;

    typedef itk::BarycentricCombination< VectorOfPoints, double * > BarycentricCalculatorType;
    BarycentricCalculatorType barycentreCalculator;
    combination = barycentreCalculator.Evaluate( points, w );
    std::cout << "Test for Barycentric combination of a VectorContainer of Points" << std::endl;
    for(unsigned int i=0; i<N; i++ )
    {
      if( std::fabs( combination[i] - (K/3.0) ) > tolerance )
      {
        std::cerr << "Failure to compute Barycentric combination" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Test for Barycentric combination of a VectorContainer of Points PASSED" << std::endl;
  }

  return EXIT_SUCCESS;
}
