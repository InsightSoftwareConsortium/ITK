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

#include <fstream>

#include "itkSymmetricEigenSystem.h"
#include "itkStdStreamStateSave.h"

int itkSymmetricEigenSystemTest(int , char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  typedef itk::SymmetricEigenSystem< double, 2 > EigenSystemType;

  EigenSystemType::Pointer eigen = EigenSystemType::New();

  EigenSystemType::MatrixType mat;
  mat.GetVnlMatrix().put(0, 0, 814.95741);
  mat.GetVnlMatrix().put(0, 1, 38.40308);
  mat.GetVnlMatrix().put(1, 0, 38.40308);
  mat.GetVnlMatrix().put(1, 1, 817.64446);
  EigenSystemType::EigenValueArrayType eigenValues;
  eigenValues[0] = 854.7275;
  eigenValues[1] = 777.8744;

  EigenSystemType::EigenVectorArrayType eigenVectors;
  eigenVectors[0][0] = 0.6946354;
  eigenVectors[0][1] = 0.7193620;
  eigenVectors[1][0] = 0.7193620;
  eigenVectors[1][1] = -0.6946354;

  double precision = 0.0000001;

  eigen->SetMatrix(&mat);
  eigen->Update();

  std::cout << "Matrix: " << mat << std::endl;
  double temp;
  std::cout.setf(std::ios::scientific, std::ios::floatfield);

  for ( unsigned int i = 0; i < 2; i++ )
    {
      temp = (*(eigen->GetEigenValues()))[i];
      std::cout << "eigen value = " << temp << std::endl;
      if ( itk::Math::abs(1 - itk::Math::abs(temp / eigenValues[i])) > precision )
        {
          std::cout << "wrong eigen value "
                    << itk::Math::abs(1 - (temp / eigenValues[i]))
                    << std::endl;
          return EXIT_FAILURE;
        }
    }

  for ( unsigned int i = 0; i < 2; i++ )
    {
      std::cout << "eigen vector = ";
      double dotProduct = 0.0;
      for ( unsigned int j = 0; j < 2; j++ )
        {
          temp = (*(eigen->GetEigenVectors()))[i][j];
          std::cout << temp << " ";

          dotProduct += temp * eigenVectors[i][j];
        }

      if ( itk::Math::abs(itk::Math::abs(dotProduct) - 1 ) > precision )
        {
          std::cout << "wrong eigen vector " << dotProduct << std::endl;
          return EXIT_FAILURE;
        }

      std::cout << std::endl;
    }

  std::cout << "Test succeeded." << std::endl;
  return EXIT_SUCCESS;
}
