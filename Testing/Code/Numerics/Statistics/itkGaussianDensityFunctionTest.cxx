/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDensityFunctionTest.cxx
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

#include "itkGaussianDensityFunction.h"
#include "itkVector.h"

int itkGaussianDensityFunctionTest(int, char* [] ) 
{
  std::cout << "itkGaussianDensityFunctionTest Test \n \n"; 

  typedef itk::Vector< double, 1 >   MeasurementVectorType;

  typedef itk::Statistics::GaussianDensityFunction< 
                             MeasurementVectorType 
                                          > DensityFunctionType;

  typedef DensityFunctionType::MeasurementVectorType MeasurementVectorType;
  typedef DensityFunctionType::CovarianceType        CovarianceType;
  typedef DensityFunctionType::MeanType              MeanType;

  MeanType mean(1);           // size = 1 because this is a scalar case.
  CovarianceType  covariance;

  covariance.SetSize( 1, 1 ); // because this is a scalar case
    
  const double Sigma = 7.0;

  mean[0] = 19.0;
  covariance[0][0] = Sigma * Sigma;

  DensityFunctionType::Pointer densityFunction = DensityFunctionType::New();

  densityFunction->SetMean( &mean );
  densityFunction->SetCovariance( &covariance );

  MeasurementVectorType inputValue;
  inputValue[0] = mean[0];

  const double value1 = densityFunction->Evaluate( inputValue );

  const double PI = 4.0 * atan( 1.0 );

  const double gaussianNorm = 1.0 / ( sqrt( 2 * PI ) * Sigma );

  if( fabs( gaussianNorm - value1 ) > 1e-6 )
    {
    std::cerr << "ERROR in computation of the Gaussian " << std::endl;
    return EXIT_FAILURE;
    }
  
  std::cout << "Evaluate at the mean = " << value1 << std::endl;
  std::cout << "Expected at the mean = " << gaussianNorm << std::endl;


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



