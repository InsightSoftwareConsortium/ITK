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

#include "VNLSparseLUSolverTraits.h"
#include "itkMath.h" // itk::Math::abs

#include <iostream>
#include <cstdlib>

template< class TVector >
bool VectorsEquals( const TVector & v1,
                    const TVector & v2,
                    const typename TVector::element_type & tolerance )
{
  if( v1.size() != v2.size() )
    {
    std::cerr << "Error: v1.size() != v2.size()" << std::endl;
    return false;
    }

  for( unsigned int i=0; i<v1.size(); i++ )
    {
    if( itk::Math::abs( v1(i) - v2(i) ) > tolerance )
      {
      std::cerr << "Error: itk::Math::abs( v1(" << i << ") - v2(" << i << ") ) > " << tolerance << std::endl;
      return false;
      }
    }

  return true;
}

int VNLSparseLUSolverTraitsTest(int, char* [] )
{
  /**
   * Define an sparse LU solver traits type that operates over sparse matrices and
   * vectors of type "double"
   */
  typedef double                                    CoordinateType;
  typedef VNLSparseLUSolverTraits< CoordinateType > SolverTraits;
  typedef SolverTraits::MatrixType                  MatrixType;
  typedef SolverTraits::VectorType                  VectorType;

  /**
   * Build the linear system to solve
   */
  unsigned int N = 3;
  VectorType Bx = SolverTraits::InitializeVector( N );
  Bx.fill( 0. );
  Bx[ 0 ] = 2.1;

  VectorType By = SolverTraits::InitializeVector( N );
  By.fill( 0. );
  By[ 1 ] = 1.1;
  By[ 2 ] = -3.;

  VectorType Bz = SolverTraits::InitializeVector( N );
  Bz.fill( 0. );
  Bz[ 0 ] = 19.4;
  Bz[ 1 ] = -4.3;

  MatrixType A = SolverTraits::InitializeSparseMatrix( N, N );
  SolverTraits::FillMatrix( A, 0, 0, 2 );
  SolverTraits::FillMatrix( A, 0, 1, -1 );
  SolverTraits::FillMatrix( A, 1, 0, -1 );
  SolverTraits::FillMatrix( A, 1, 1, 2 );
  SolverTraits::FillMatrix( A, 1, 2, -1 );
  SolverTraits::FillMatrix( A, 2, 1, -1 );
  SolverTraits::FillMatrix( A, 2, 2, 2 );

  /**
   * Define the tolerance and expected results
   */
  CoordinateType tolerance = 1e-9;

  VectorType Xexpected( N );
  Xexpected(0) = 1.575;
  Xexpected(1) = 1.05;
  Xexpected(2) = 0.525;

  VectorType Yexpected( N );
  Yexpected(0) = -0.2;
  Yexpected(1) = -0.4;
  Yexpected(2) = -1.7;

  VectorType Zexpected( N );
  Zexpected(0) = 12.4;
  Zexpected(1) = 5.4;
  Zexpected(2) = 2.7;

 /**
  * Test 1: Check the result of A * X = Bx
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    SolverTraits::Solve( A, Bx, X );
    if( !VectorsEquals( X, Xexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

 /**
  * Test 2: Check the result of A * X = Bx, A * Y = By
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    VectorType Y = SolverTraits::InitializeVector( N );
    SolverTraits::Solve( A, Bx, X );
    SolverTraits::Solve( A, By, Y );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

 /**
  * Test 3: Check the result of A * X = Bx, A * Y = By, A * Z = Bz
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    VectorType Y = SolverTraits::InitializeVector( N );
    VectorType Z = SolverTraits::InitializeVector( N );
    SolverTraits::Solve( A, Bx, X );
    SolverTraits::Solve( A, By, Y );
    SolverTraits::Solve( A, Bz, Z );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) ||
        !VectorsEquals( Z, Zexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

 /**
  * Test 4: Check the result of A * X = Bx (reuse the decomposed matrix for multiple back-substitutions)
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    SolverTraits::SolverType solver( A );

    // First back-substitution
    SolverTraits::Solve( solver, Bx, X );
    if( !VectorsEquals( X, Xexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve( solver, Bx, X );
    if( !VectorsEquals( X, Xexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

 /**
  * Test 5: Check the result of A * X = Bx, A * Y = By (reuse the decomposed matrix for multiple back-substitutions)
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    VectorType Y = SolverTraits::InitializeVector( N );
    SolverTraits::SolverType solver( A );

    // First back-substitution
    SolverTraits::Solve( solver, Bx, X );
    SolverTraits::Solve( solver, By, Y );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve( solver, Bx, X );
    SolverTraits::Solve( solver, By, Y );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

 /**
  * Test 6: Check the result of A * X = Bx, A * Y = By, A * Z = Bz (reuse the decomposed matrix for multiple back-substitutions)
  */
  {
    VectorType X = SolverTraits::InitializeVector( N );
    VectorType Y = SolverTraits::InitializeVector( N );
    VectorType Z = SolverTraits::InitializeVector( N );
    SolverTraits::SolverType solver( A );

    // First back-substitution
    SolverTraits::Solve( solver, Bx, X );
    SolverTraits::Solve( solver, By, Y );
    SolverTraits::Solve( solver, Bz, Z );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) ||
        !VectorsEquals( Z, Zexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve( solver, Bx, X );
    SolverTraits::Solve( solver, By, Y );
    SolverTraits::Solve( solver, Bz, Z );
    if( !VectorsEquals( X, Xexpected, tolerance ) ||
        !VectorsEquals( Y, Yexpected, tolerance ) ||
        !VectorsEquals( Z, Zexpected, tolerance ) )
      {
      return EXIT_FAILURE;
      }
  }

  return EXIT_SUCCESS;
}
