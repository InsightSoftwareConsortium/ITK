/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMatrixTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>

#include "itkMatrix.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"

#include "vnl/vnl_vector_fixed.h"


int main() 
{


  typedef   float                                NumericType;
  typedef   itk::Matrix<NumericType,3,3>          MatrixType;
  typedef   itk::Vector<NumericType,3>            VectorType;
  typedef   itk::Point<NumericType,3>             PointType;
  typedef   itk::CovariantVector<NumericType,3>   CovariantVectorType;

  typedef   vnl_vector_fixed<NumericType,3>       vnlVectorType;



  MatrixType matrix;

  matrix.set_identity();

  VectorType v1;
  v1 = 3,4,5;
  
  VectorType resultVector = matrix * v1;
  std::cout << resultVector[0] << ", ";
  std::cout << resultVector[1] << ", ";
  std::cout << resultVector[2] << std::endl;

  PointType p1;
  p1 = 3,4,5;
  
  PointType resultPoint = matrix * p1;
  std::cout << resultPoint[0] << ", ";
  std::cout << resultPoint[1] << ", ";
  std::cout << resultPoint[2] << std::endl;

  CovariantVectorType cv1;
  cv1 = 3,4,5;
  
  CovariantVectorType resultCovariantVector = matrix * cv1;
  std::cout << resultCovariantVector[0] << ", ";
  std::cout << resultCovariantVector[1] << ", ";
  std::cout << resultCovariantVector[2] << std::endl;

  vnlVectorType v2;
  v2 = 3,4,5;
  
  vnlVectorType resultVnlVector = matrix * v2;
  std::cout << resultVnlVector[0] << ", ";
  std::cout << resultVnlVector[1] << ", ";
  std::cout << resultVnlVector[2] << std::endl;

  MatrixType matrix2;
  matrix2.set_identity();
  matrix2(0,0) = 10;

  MatrixType matrixProduct;

  matrixProduct = matrix * matrix2;

  MatrixType matrix3;
  matrix3 = matrix.GetInverse();

  MatrixType matrix4;
  matrix4 = matrix.GetTranspose();

  return 0;
}
