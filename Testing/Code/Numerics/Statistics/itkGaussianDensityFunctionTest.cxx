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

  MeanType mean;
  CovarianceType  covariance;

  mean[0] = 19.0;
  covariance[0][0] = 1.0;

  DensityFunctionType::Pointer densityFunction = DensityFunctionType::New();

  densityFunction->SetMean( &mean );
  densityFunction->SetCovariance( &covariance );

  MeasurementVectorType inputValue;
  inputValue[0] = mean[0];

  const double value1 = densityFunction->Evaluate( inputValue );

  std::cout << "Evaluate at the mean = " << value1 << std::endl;


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



