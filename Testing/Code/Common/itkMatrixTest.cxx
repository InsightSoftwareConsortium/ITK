/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMatrixTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

  matrix.Fill( 0.0 );

  matrix.SetIdentity();

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

  return 0;
}
