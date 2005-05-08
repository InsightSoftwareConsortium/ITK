/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEigenAnalysisTest.cxx
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

#include "itkSymmetricEigenAnalysis.h"
#include "vnl/vnl_matrix.h"
#include "itkFixedArray.h"
#include "itkMatrix.h"


int itkSymmetricEigenAnalysisTest(int, char* [] )
{
  // Test SymmetricEigenAnalysis class with symmetric matrices 

  // Test using vnl_matrix
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< vnl_matrix, itk::FixedArray, itk::Matrix >" 
    << std::endl;
  typedef vnl_matrix< double > InputMatrixType;
  typedef itk::FixedArray< double, 6 > EigenValuesArrayType;
  typedef itk::Matrix< double, 6, 6 > EigenVectorMatrixType;
  typedef itk::SymmetricEigenAnalysis< InputMatrixType,  
      EigenValuesArrayType, EigenVectorMatrixType > SymmetricEigenAnalysisType;
   
  double Sdata[36] = {
   30.0000,   -3.4273,   13.9254,   13.7049,   -2.4446,   20.2380,
   -3.4273,   13.7049,   -2.4446,    1.3659,    3.6702,   -0.2282,
   13.9254,   -2.4446,   20.2380,    3.6702,   -0.2282,   28.6779,
   13.7049,    1.3659,    3.6702,   12.5273,   -1.6045,    3.9419,
   -2.4446,    3.6702,   -0.2282,   -1.6045,    3.9419,    2.5821,
   20.2380,   -0.2282,   28.6779,    3.9419,    2.5821,   44.0636,
  };
  
  InputMatrixType S(Sdata, 6,6);
  EigenValuesArrayType eigenvalues;
  EigenVectorMatrixType eigenvectors;
  SymmetricEigenAnalysisType symmetricEigenSystem(6);
  
  symmetricEigenSystem.ComputeEigenValuesAndVectors(S, eigenvalues, eigenvectors );

  std::cout << "EigenValues: " << eigenvalues << std::endl;
  std::cout << "EigenVectors (each row is an an eigen vector): " << std::endl;
  std::cout << eigenvectors << std::endl;

  double eigvec3[6] = { 0.5236407,  -0.0013422,  -0.4199706,  -0.5942299,   0.4381326,   0.0659837 };
  double eigvals[6]= {0.170864, 2.16934, 3.79272, 15.435, 24.6083, 78.2994}; 
  
  double tolerance = 0.01;
  for( unsigned int i=0; i<6; i++ )
    {
    if (vnl_math_abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    
     if (vnl_math_abs( eigvec3[i] - eigenvectors[2][i] ) > tolerance)
      {
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "[TEST PASSED]" << std::endl;
return EXIT_SUCCESS;

}


  
