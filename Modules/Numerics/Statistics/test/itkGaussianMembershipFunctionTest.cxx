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
  std::cout << "***" << std::endl;
  std::cout << "Exception TEST: " << std::endl;
  try
    {
    MeasurementVectorSizeType measurementVector2 = MeasurementVectorSize + 1;
    function->SetMeasurementVectorSize( measurementVector2 );
    std::cerr << "Exception should have been thrown since we are trying to resize\
                  non-resizeable measurement vector type " << std::endl;
    //return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
    }


  //Test if the membership function value computed is correct
  MembershipFunctionType::MeanVectorType mean;
  ::itk::NumericTraits<MembershipFunctionType::MeanVectorType>::SetLength( mean,
    MeasurementVectorSize);
  mean[0] = 1.5;
  function->SetMean( mean );

  const double tolerance = 0.001;

  if( std::fabs( function->GetMean()[0] - mean[0]) > tolerance )
    {
    std::cerr << "Error in GetMean() method" << std::endl;
    return EXIT_FAILURE;
    }

  MembershipFunctionType::CovarianceMatrixType covariance;
  covariance.SetSize(MeasurementVectorSize,MeasurementVectorSize);
  covariance.SetIdentity();
  function->SetCovariance( covariance );

  if( function->GetCovariance() != covariance )
    {
    std::cerr<< "Get/SetCovariance() failure \n" << std::endl;
    return EXIT_FAILURE;
    }

  MeasurementVectorType measurement;
  ::itk::NumericTraits<MeasurementVectorType>::SetLength( measurement, MeasurementVectorSize);
  measurement[0] = 1.5;

  double trueValue = 0.3989;
  double distanceComputed = function->Evaluate( measurement );

  if( std::fabs( distanceComputed - trueValue) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue
              << ", ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
