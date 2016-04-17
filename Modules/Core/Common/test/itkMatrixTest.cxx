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

#include "itkMatrix.h"
#include "itkMath.h"

int itkMatrixTest(int, char* [] )
{
  typedef   float                                 NumericType;
  typedef   itk::Matrix<NumericType,3,3>          MatrixType;
  typedef   itk::Vector<NumericType,3>            VectorType;
  typedef   itk::Point<NumericType,3>             PointType;
  typedef   itk::CovariantVector<NumericType,3>   CovariantVectorType;

  typedef   vnl_vector_fixed<NumericType,3>       vnlVectorType;

  MatrixType matrix;
  matrix.Fill( 0.0 );
  matrix.SetIdentity();

  VectorType v1;
  v1[0] = 3;
  v1[1] = 4;
  v1[2] = 5;

  VectorType resultVector = matrix * v1;
  std::cout << resultVector[0] << ", ";
  std::cout << resultVector[1] << ", ";
  std::cout << resultVector[2] << std::endl;

  PointType::ValueType p1Init[3] = {3,4,5};
  PointType p1 = p1Init;

  PointType resultPoint = matrix * p1;
  std::cout << resultPoint[0] << ", ";
  std::cout << resultPoint[1] << ", ";
  std::cout << resultPoint[2] << std::endl;

  CovariantVectorType::ValueType cv1Init[3] = {3,4,5};
  CovariantVectorType cv1 = cv1Init;

  CovariantVectorType resultCovariantVector = matrix * cv1;
  std::cout << resultCovariantVector[0] << ", ";
  std::cout << resultCovariantVector[1] << ", ";
  std::cout << resultCovariantVector[2] << std::endl;

  vnlVectorType v2;
  v2[0] = 3;
  v2[1] = 4;
  v2[2] = 5;

  vnlVectorType resultVnlVector = matrix * v2;
  std::cout << resultVnlVector[0] << ", ";
  std::cout << resultVnlVector[1] << ", ";
  std::cout << resultVnlVector[2] << std::endl;

  MatrixType matrix2;
  matrix2.SetIdentity();
  matrix2.GetVnlMatrix()(0,0) = 10;

  MatrixType matrixProduct;
  matrixProduct = matrix * matrix2;

  MatrixType matrix3;
  matrix3 = matrix.GetInverse();

  MatrixType matrix4;
  matrix4 = matrix.GetTranspose();

  MatrixType matrix5;
  matrix5.Fill( 1.7 );

  const NumericType value = 2;
  matrix5[1][1] = value;
  if( itk::Math::NotExactlyEquals(matrix5[1][1], value) )
    {
    std::cerr << "Problem accessing matrix element " << std::endl;
    return EXIT_FAILURE;
    }

  // Test access with the operator()(row,col)
  const NumericType value2 = 19;
  matrix5(1,1) = value2;
  if( itk::Math::NotExactlyEquals(matrix5[1][1], value2) )
    {
    std::cerr << "Problem accessing matrix element " << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::NotExactlyEquals(matrix5(1,1), value2) )
    {
    std::cerr << "Problem accessing matrix element " << std::endl;
    return EXIT_FAILURE;
    }


  MatrixType matrix6 = matrix5 * 2.5;

  matrix6 *= 1.3;


  // This was added after a bug in operator*() was reported on the users list.
  std::cout << "Testing products in non-square matrices" << std::endl;

  try
    {
      itk::Matrix<double,2,2> m1;
      itk::Matrix<double,2,2> m2;

      for(unsigned int i=0; i<2;i++ )
        {
        for( unsigned int j=0; j<2; j++ )
          {
          m1[i][j]=i+j;
          }
        }

      std::cout << "m1="  << std::endl;
      std::cout << m1 << std::endl;


      for(unsigned int i=0; i<2; i++)
        {
        for(unsigned int j=0; j<2; j++)
          {
          m2[i][j]=i+j;
          }
        }

      std::cout << "m2=" << std::endl;
      std::cout << m2 << std::endl;


      std::cout << "VNL * VNL Multiplication result: " << std::endl;
      std::cout << m1.GetVnlMatrix()*m2.GetVnlMatrix() << std::endl;

      std::cout << "ITK * VNL Multiplication result: " << std::endl;
      std::cout << m1 * m2.GetVnlMatrix() << std::endl;

      itk::Matrix<double,2,2> m3;
      itk::Matrix<double,2,3> m4;

      for(unsigned int i=0; i<2; i++ )
          {
          for(unsigned int j=0; j<2; j++)
            {
            m3[i][j]=i+j;
            }
          }

      std::cout << "m3="  << std::endl;
      std::cout << m3 << std::endl;


      for(unsigned int i=0; i<2; i++ )
        {
        for(unsigned int j=0; j<3; j++ )
          {
          m4[i][j]=i+j;
          }
        }

      std::cout << "m4=" << std::endl;
      std::cout << m4 << std::endl;


      std::cout << "VNL * VNL Multiplication result: " << std::endl;
      std::cout << m3.GetVnlMatrix()*m4.GetVnlMatrix() << std::endl;

      std::cout << "ITK * VNL Multiplication result: " << std::endl;
      std::cout << m3 * m4.GetVnlMatrix() << std::endl;

    }
  catch ( itk::ExceptionObject & e)
    {
    std::cerr<<"Exception caught in test..."<<std::endl;
    std::cerr<< e <<std::endl;
    return EXIT_FAILURE;
    }


  { // Test for Matrix addition and subtraction
    const unsigned int nc = 4;
    const unsigned int nr = 3;

    typedef itk::Matrix<double, nr, nc>  AddSubtractMatrixType;

    AddSubtractMatrixType m1;
    AddSubtractMatrixType m2;

    // fill the matrices with something
    {
    for(unsigned int r=0; r < nr; r++)
      {
      for(unsigned int c=0; c < nc; c++)
        {
        const double fr = (double)r;
        const double fc = (double)c;
        m1[r][c] = fr + fc;
        m2[r][c] = fr - fc;
        }
      }
    }

    AddSubtractMatrixType m3;
    m3 = m1 + m2;

    AddSubtractMatrixType m4;
    m4 = m1 - m2;

    std::cout << "Results of ITK matrix addition" << std::endl;
    std::cout << "M1 = " << std::endl << m1 << std::endl;
    std::cout << "M2 = " << std::endl << m2 << std::endl;
    std::cout << "M1+M2 = " << std::endl << m3 << std::endl;
    std::cout << "M1-M2 = " << std::endl << m4 << std::endl;

    // Check the addition and subtraction values
    {
    const double tolerance = 1e-7;
    for(unsigned int r=0; r < nr; r++)
      {
      for(unsigned int c=0; c < nc; c++)
        {
        if( std::fabs( m3[r][c] - 2*r ) > tolerance )
          {
          std::cerr << "Addition failed !" << std::endl;
          std::cerr << "M["<< r << "][" << c << "] = ";
          std::cerr << m3[r][c] << std::endl;
          return EXIT_FAILURE;
          }
        if( std::fabs( m4[r][c] - 2*c ) > tolerance )
          {
          std::cerr << "Subtraction failed !" << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }

    m3 -= m2;
    m4 += m2;

     // Check the in-place addition and subtraction values
    {
    const double tolerance = 1e-7;
    for(unsigned int r=0; r < nr; r++)
      {
      for(unsigned int c=0; c < nc; c++)
        {
        if( std::fabs( m3[r][c] - m1[r][c] ) > tolerance )
          {
          std::cerr << "In-place addition failed !" << std::endl;
          std::cerr << m3 << std::endl;
          return EXIT_FAILURE;
          }
        if( std::fabs( m4[r][c] - m1[r][c] ) > tolerance )
          {
          std::cerr << "In-place subtraction failed !" << std::endl;
          std::cerr << m4 << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }
  }

  {
  // Test for vnl_matrix_fixed assignment and construction
  MatrixType matrixA;

  int counter = 0;
  for( unsigned int row=0; row < 3; row++)
    {
    for( unsigned int col=0; col < 3; col++ )
      {
      matrixA[row][col] = counter++;
      }
    }

  MatrixType::InternalMatrixType vnlMatrixA = matrixA.GetVnlMatrix();

  MatrixType matrixB( vnlMatrixA ); // Test constructor

    { // verify values
    const double tolerance = 1e-7;
    for( unsigned int row=0; row < 3; row++)
      {
      for( unsigned int col=0; col < 3; col++ )
        {
        if( std::abs( matrixB[row][col] - matrixA[row][col] ) > tolerance )
          {
          std::cerr << "constructor from vnl_matrix failed ! " << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }

  MatrixType matrixC;
  matrixC = vnlMatrixA; // Test assignment

    { // verify values
    const double tolerance = 1e-7;
    for( unsigned int row=0; row < 3; row++)
      {
      for( unsigned int col=0; col < 3; col++ )
        {
        if( std::abs( matrixC[row][col] - matrixA[row][col] ) > tolerance )
          {
          std::cerr << "assignment from vnl_matrix failed ! " << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }

  }

  typedef   itk::Matrix<NumericType,7,7> LargeMatrixType;
  LargeMatrixType matrixBad;
  matrixBad.Fill( 2.0);
  bool caught = false;
  try
    {
    matrixBad.GetInverse();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cout << "Caught expected exception!" << std::endl;
    std::cout << excp;
    caught = true;
    }
  if (!caught)
    {
    std::cout << "Failed to catch expected exception!" << std::endl;
    return EXIT_FAILURE;
    }

  matrixBad.SetIdentity();
  try
    {
    matrixBad.GetInverse();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cout << "Caught unexpected exception!" << std::endl;
    std::cout << excp;
    return EXIT_FAILURE;
    }

  {
  /*
   *  This tries to invert a matrix close to
   *     1.0156e+00   3.7242e-33   1.8034e-17
   *    -1.5263e-17   2.4781e-16   1.2000e+00
   *    -1.4363e-40  -1.0156e+00   2.9279e-16
   *
   *  Matlab suggest that the inverse of this matrix is close to
   *
   *     9.8462e-01  -1.4797e-17  -5.6092e-40
   *     3.6105e-33   2.4024e-16  -9.8462e-01
   *     1.2524e-17   8.3334e-01   2.0333e-16
   *
   *  However, it has been reported that vnl_matrix_inverse may produce a warning
   *
   *    vnl/algo/vnl_svd.txx: suspicious return value (3) from SVDC
   *    vnl/algo/vnl_svd.txx: M is 3x3
   *
   *  and will then only return NaN values
   *
   */

   itk::Matrix<double,3,3> invertibleMatrix;
   invertibleMatrix[0][0] = 1.015625;
   invertibleMatrix[0][1] = 3.7241876106217399000472402087113071242494464386448e-33;
   invertibleMatrix[0][2] = 1.8034176825630083413048304619152872874110471457243e-17;
   invertibleMatrix[1][0] = -1.5263338494897929595479901809795819644932635128498e-17;
   invertibleMatrix[1][1] = 2.4780806492783651398537081433914863737300038337708e-16;
   invertibleMatrix[1][2] = 1.1999969482421875;
   invertibleMatrix[2][0] = -1.4362888869790077531846951109846653370783300825652e-40;
   invertibleMatrix[2][1] = -1.015625;
   invertibleMatrix[2][2] = 2.9279401122629200669017501823532256821636110544205e-16;

   itk::Matrix<double,3,3> inverseMatrixMatlab;
   inverseMatrixMatlab[0][0] = 9.8462e-01;
   inverseMatrixMatlab[0][1] = -1.4797e-17;
   inverseMatrixMatlab[0][2] = -5.6092e-40;
   inverseMatrixMatlab[1][0] = 3.6105e-33;
   inverseMatrixMatlab[1][1] = 2.4024e-16;
   inverseMatrixMatlab[1][2] = -9.8462e-01;
   inverseMatrixMatlab[2][0] = 1.2524e-17;
   inverseMatrixMatlab[2][1] = 8.3334e-01;
   inverseMatrixMatlab[2][2] = 2.0333e-16;

   std::cout << "Testing matrix inversion for " << std::endl << invertibleMatrix << std::endl;

   const vnl_matrix_fixed<double,3,3> invertedMatrix = invertibleMatrix.GetInverse();

   std::cout << "Inverted to " << std::endl << invertedMatrix << std::endl;

   std::cout << "Matlab inverse " << std::endl << inverseMatrixMatlab << std::endl;

   unsigned int num_nans(0);
   for ( unsigned int i = 0; i < 3; ++i )
     {
     for ( unsigned int j = 0; j < 3; ++j )
       {
         if ( itk::Math::isnan(invertedMatrix[i][j]) ) ++num_nans;
       }
     }


   const double relative_error = ( invertedMatrix - inverseMatrixMatlab.GetVnlMatrix() ).frobenius_norm()
      / inverseMatrixMatlab.GetVnlMatrix().frobenius_norm();

   std::cout << "Relative error compared to Matlab " << std::endl << relative_error << std::endl;

   if ( num_nans>0 || relative_error>1e-4 )
     {
     std::cout << "Matrix could not be inverted" << std::endl;
     return EXIT_FAILURE;
     }
  }


  std::cout << "Test Passed !" << std::endl;
  return EXIT_SUCCESS;
}
