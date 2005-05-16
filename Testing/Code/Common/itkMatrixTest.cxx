/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrixTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkMatrix.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"

#include "vnl/vnl_vector_fixed.h"


int itkMatrixTest(int, char* [] ) 
{


  typedef   float                                NumericType;
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
  if( matrix5[1][1] != value )
    {
    std::cerr << "Problem accessing matrix element " << std::endl;
    return EXIT_FAILURE;
    }

  // Test access with the operator()(row,col)
  const NumericType value2 = 19;
  matrix5(1,1) = value2;
  if( matrix5[1][1] != value2 )
    {
    std::cerr << "Problem accessing matrix element " << std::endl;
    return EXIT_FAILURE;
    }
  if( matrix5(1,1) != value2 )
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
       
      for(unsigned int i=0; i<2 ;i++ ) 
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
      std::cout << m1*m2.GetVnlMatrix() << std::endl;



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
      std::cout << m3*m4.GetVnlMatrix() << std::endl;
      
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

    typedef itk::Matrix<double, nr, nc>  MatrixType;
    
    MatrixType m1;
    MatrixType m2;
    
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

    MatrixType m3;
    m3 = m1 + m2;

    MatrixType m4;
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
        if( fabs( m3[r][c] - 2*r ) > tolerance ) 
          {
          std::cerr << "Addition failed !" << std::endl;
          std::cerr << "M["<< r << "][" << c << "] = ";
          std::cerr << m3[r][c] << std::endl;
          return EXIT_FAILURE;
          }
        if( fabs( m4[r][c] - 2*c ) > tolerance ) 
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
        if( fabs( m3[r][c] - m1[r][c] ) > tolerance ) 
          {
          std::cerr << "In-place addition failed !" << std::endl;
          std::cerr << m3 << std::endl;
          return EXIT_FAILURE;
          }
        if( fabs( m4[r][c] - m1[r][c] ) > tolerance ) 
          {
          std::cerr << "In-place subtraction failed !" << std::endl;
          std::cerr << m4 << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }

   


  }


  std::cout << "Test Passed !" << std::endl;

  return EXIT_SUCCESS;
}


