/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSymmetricEigenSystemTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWin32Header.h"

#include <fstream>

#include "itkSymmetricEigenSystem.h"

int itkSymmetricEigenSystemTest(int argc, char* argv[] )
{
  typedef itk::SymmetricEigenSystem< double, 2 > EigenSystemType ;
  
  EigenSystemType::Pointer eigen = EigenSystemType::New() ;

  EigenSystemType::MatrixType mat ;
  mat.GetVnlMatrix().put(0, 0, 814.95741) ;
  mat.GetVnlMatrix().put(0, 1, 38.40308) ;
  mat.GetVnlMatrix().put(1, 0, 38.40308) ;
  mat.GetVnlMatrix().put(1, 1, 817.64446) ;
  EigenSystemType::EigenValueArrayType eigenValues ;
  eigenValues[0] = 854.7275 ;
  eigenValues[1] = 777.8744 ;

  EigenSystemType::EigenVectorArrayType eigenVectors ;
  eigenVectors[0][0] = 0.6946354 ;
  eigenVectors[0][1] = 0.7193620 ;
  eigenVectors[1][0] = 0.7193620 ;
  eigenVectors[1][1] = -0.6946354 ;

  double precision = 0.0000001 ;

  eigen->SetMatrix(&mat) ;
  eigen->Update() ;

  std::cout << "Matrix: " << mat << std::endl ;
  double temp ;
  std::cout.setf(std::ios::scientific, std::ios::floatfield) ;
  
  for ( unsigned int i = 0 ; i < 2 ; i++ )
    {
      temp = (*(eigen->GetEigenValues()))[i]  ;
      std::cout << "eigen value = " << temp << std::endl;
      if ( vnl_math_abs(1 - vnl_math_abs(temp / eigenValues[i])) > precision )
        {
          std::cout << "wrong eigen value " 
                    << vnl_math_abs(1 - (temp / eigenValues[i])) 
                    << std::endl ; 
          return EXIT_FAILURE;
        }
    }

  for ( unsigned int i = 0 ; i < 2 ; i++ )
    {
      std::cout << "eigen vector = " ; 
      double dotProduct = 0.0 ;
      for ( unsigned int j = 0 ; j < 2 ; j++ )
        {
          temp = (*(eigen->GetEigenVectors()))[i][j] ;
          std::cout << temp << " " ;

          dotProduct += temp * eigenVectors[i][j] ;
        }

      if ( vnl_math_abs(vnl_math_abs(dotProduct) - 1 ) > precision )
        {
          std::cout << "wrong eigen vector " << dotProduct << std::endl ; 
          return EXIT_FAILURE;
        }

      std::cout << std::endl ;
    }

  std::cout << "Test succeeded." << std::endl;
  return EXIT_SUCCESS ;
}
