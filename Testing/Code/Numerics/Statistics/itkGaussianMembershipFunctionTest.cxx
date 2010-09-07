/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianMembershipFunctionTest.cxx
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
#include "itkGaussianMembershipFunction.h"

int itkGaussianMembershipFunctionTest(int, char* [] )
{
  const unsigned int MeasurementVectorSize = 1;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::GaussianMembershipFunction< MeasurementVectorType >   MembershipFunctionType;
  typedef MembershipFunctionType::MeasurementVectorSizeType MeasurementVectorSizeType;

  MembershipFunctionType::Pointer function = MembershipFunctionType::New();
  std::cout << function->GetNameOfClass() << std::endl;

  function->Print(std::cout);

  function->SetMeasurementVectorSize( MeasurementVectorSize ); // for code coverage

  if( function->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //Test if an exception will be thrown if we try to resize the measurement vector
  //size
  try
    {
    MeasurementVectorSizeType measurementVector2 = MeasurementVectorSize + 1;
    function->SetMeasurementVectorSize( measurementVector2 );
    std::cerr << "Exception should have been thrown since we are trying to resize\
                  non-resizeable measurement vector type " << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
    }


  //Test if the membership function value computed is correct
  MembershipFunctionType::MeanType mean;
  ::itk::Statistics::MeasurementVectorTraits::SetLength( mean, MeasurementVectorSize);
  mean[0] = 1.5;
  function->SetMean( mean );

  const double tolerance = 0.001;

  if( vcl_fabs( function->GetMean()[0] - mean[0]) > tolerance )
    {
    std::cerr << "Error in GetMean() method" << std::endl;
    return EXIT_FAILURE;
    }

  MembershipFunctionType::CovarianceType covariance;
  covariance.SetSize(MeasurementVectorSize,MeasurementVectorSize);
  covariance.SetIdentity();
  function->SetCovariance( covariance );

  if( function->GetCovariance() != covariance )
    {
    std::cerr<< "Get/SetCovariance() failure \n" << std::endl;
    return EXIT_FAILURE;
    }

  MeasurementVectorType measurement;
  ::itk::Statistics::MeasurementVectorTraits::SetLength( measurement, MeasurementVectorSize);
  measurement[0] = 1.5;

  double trueValue = 0.3989;
  double distanceComputed = function->Evaluate( measurement );

  if( vcl_fabs( distanceComputed - trueValue) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue
              << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
