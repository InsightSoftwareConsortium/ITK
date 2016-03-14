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

#include "itkSymmetricSecondRankTensor.h"


int itkSymmetricEigenAnalysisTest(int, char* [] )
{
  // Test SymmetricEigenAnalysis class with symmetric matrices

  {
  // Test using vnl_matrix
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< vnl_matrix, itk::FixedArray, itk::Matrix >"
    << std::endl;
  typedef vnl_matrix< double >                      InputMatrixType;
  typedef itk::FixedArray< double, 6 >              EigenValuesArrayType;
  typedef itk::Matrix< double, 6, 6 >               EigenVectorMatrixType;
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
    if (itk::Math::abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }

    if (itk::Math::abs( eigvec3[i] - itk::Math::sgn0( eigenvectors[2][0] )*eigenvectors[2][i] ) > tolerance)
      {
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  {
  // Test using itk Matrix
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< itk::Matrix, itk::FixedArray, itk::Matrix >"
    << std::endl;
  typedef itk::Matrix< double, 6, 6 >               InputMatrixType;
  typedef itk::FixedArray< double, 6 >              EigenValuesArrayType;
  typedef itk::Matrix< double, 6, 6 >               EigenVectorMatrixType;
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

  InputMatrixType S;

  for(unsigned int row=0; row<6; row++)
    {
    for(unsigned int col=0; col<6; col++)
      {
      S[row][col] = Sdata[ row * 6 + col ];
      }
    }

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
    if (itk::Math::abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }

     if (itk::Math::abs( eigvec3[i] - itk::Math::sgn0( eigenvectors[2][0] )* eigenvectors[2][i] ) > tolerance)
      {
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  {
  // Test using itk SymmetricSecondRankTensor
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< itk::SymmetricSecondRankTensor, itk::FixedArray, itk::Matrix >"
    << std::endl;
  typedef itk::SymmetricSecondRankTensor< double, 6 > InputMatrixType;
  typedef itk::FixedArray< double, 6 >                EigenValuesArrayType;
  typedef itk::Matrix< double, 6, 6 >                 EigenVectorMatrixType;
  typedef itk::SymmetricEigenAnalysis< InputMatrixType,
      EigenValuesArrayType, EigenVectorMatrixType >   SymmetricEigenAnalysisType;

  double Sdata[36] = {
   30.0000,   -3.4273,   13.9254,   13.7049,   -2.4446,   20.2380,
   -3.4273,   13.7049,   -2.4446,    1.3659,    3.6702,   -0.2282,
   13.9254,   -2.4446,   20.2380,    3.6702,   -0.2282,   28.6779,
   13.7049,    1.3659,    3.6702,   12.5273,   -1.6045,    3.9419,
   -2.4446,    3.6702,   -0.2282,   -1.6045,    3.9419,    2.5821,
   20.2380,   -0.2282,   28.6779,    3.9419,    2.5821,   44.0636,
  };

  InputMatrixType S;

  for(unsigned int row=0; row<6; row++)
    {
    for(unsigned int col=0; col<6; col++)
      {
      S(row,col) = Sdata[ row * 6 + col ];
      }
    }

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
    if (itk::Math::abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }

     if (itk::Math::abs( eigvec3[i] - itk::Math::sgn0( eigenvectors[2][0] )* eigenvectors[2][i] ) > tolerance)
      {
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  {
  // Test sorting when some eigenvalues are negative
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< itk::Matrix, itk::FixedArray, itk::Matrix >"
    << std::endl;
  typedef itk::Matrix< double, 3, 3 >               InputMatrixType;
  typedef itk::FixedArray< double, 3 >              EigenValuesArrayType;
  typedef itk::Matrix< double, 3, 3 >               EigenVectorMatrixType;
  typedef itk::SymmetricEigenAnalysis< InputMatrixType,
      EigenValuesArrayType, EigenVectorMatrixType > SymmetricEigenAnalysisType;

  double Sdata[9] = {
    -3.0, 0.0, 0.0,
     0.0, 5.0, 0.0,
     0.0, 0.0, -1.0,
  };

  InputMatrixType S;

  for(unsigned int row=0; row<3; row++)
    {
    for(unsigned int col=0; col<3; col++)
      {
      S(row,col) = Sdata[ row * 3 + col ];
      }
    }

  EigenValuesArrayType eigenvalues;
  EigenVectorMatrixType eigenvectors;
  SymmetricEigenAnalysisType symmetricEigenSystem(3);
  symmetricEigenSystem.SetOrderEigenMagnitudes(true);
  symmetricEigenSystem.ComputeEigenValuesAndVectors(S, eigenvalues, eigenvectors );

  std::cout << "EigenValues: " << eigenvalues << std::endl;
  std::cout << "EigenVectors (each row is an an eigen vector): " << std::endl;
  std::cout << eigenvectors << std::endl;

  double eigvec3[3] = { 0.0, 1.0, 0.0};
  double eigvals[3]= {-1.0, -3.0, 5.0};

  double tolerance = 0.01;
  for( unsigned int i=0; i<3; i++ )
    {
    if (itk::Math::abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }

     if (itk::Math::abs( eigvec3[i] - itk::Math::sgn0( eigenvectors[2][0] )* eigenvectors[2][i] ) > tolerance)
      {
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  }


  {
  // Test sorting when some eigenvalues are negative
  std::cout << "Testing ComputeEigenValuesAndVectors() "
    << "with SymmetricEigenAnalysis< itk::Matrix, itk::FixedArray, itk::Matrix >"
    << std::endl;
  typedef itk::Matrix< float, 3, 3 >               InputMatrixType;
  typedef itk::FixedArray< float, 3 >              EigenValuesArrayType;
  typedef itk::Matrix< float, 3, 3 >               EigenVectorMatrixType;
  typedef itk::SymmetricEigenAnalysis< InputMatrixType,
      EigenValuesArrayType, EigenVectorMatrixType > SymmetricEigenAnalysisType;

  float Sdata[9] = {
    -7.31129000e+00f,   2.33080000e+01f,   0.00000000e+00f,
    2.33080000e+01f,  -4.64210000e-01f,   0.00000000e+00f,
    0.00000000e+00f,   0.00000000e+00f,  -3.26256000e-06f
  };

  InputMatrixType S;

  for(unsigned int row=0; row<3; row++)
    {
    for(unsigned int col=0; col<3; col++)
      {
      S(row,col) = Sdata[ row * 3 + col ];
      }
    }

  EigenValuesArrayType eigenvalues;
  EigenVectorMatrixType eigenvectors;
  SymmetricEigenAnalysisType symmetricEigenSystem(3);
  symmetricEigenSystem.SetOrderEigenMagnitudes(true);
  symmetricEigenSystem.ComputeEigenValuesAndVectors(S, eigenvalues, eigenvectors );

  std::cout << "EigenValues: " << eigenvalues << std::endl;
  std::cout << "EigenVectors (each row is an an eigen vector): " << std::endl;
  std::cout << eigenvectors << std::endl;

  float eigvec2[3] = { 0.75674412f, -0.6537112f, 0.0f };
  float eigvals[3]= { -3.26256000e-06f,   1.96703376e+01f, -2.74458376e+01f};

  float tolerance = 0.01;
  for( unsigned int i=0; i<3; i++ )
    {
    if (itk::Math::abs( eigvals[i] - eigenvalues[i] ) > tolerance)
      {
      std::cout << "Eigen value computation failed" << std::endl;
      return EXIT_FAILURE;
      }

     if (itk::Math::abs( eigvec2[i] - itk::Math::sgn0( eigenvectors[2][0] )* eigenvectors[2][i] ) > tolerance)
      {
      std::cout << eigenvectors[2][i] << " "  << eigvec2[i]  << std::endl;
      std::cout << "Eigen vector computation failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  std::cout << "[TEST PASSED]" << std::endl;

return EXIT_SUCCESS;

}
