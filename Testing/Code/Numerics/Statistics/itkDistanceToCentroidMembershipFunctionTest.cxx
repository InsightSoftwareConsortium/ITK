/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunctionTest.cxx
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
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkObjectFactory.h"
#include "itkEuclideanDistanceMetric.h"

int itkDistanceToCentroidMembershipFunctionTest(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 3;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::DistanceToCentroidMembershipFunction<
    MeasurementVectorType >   MembershipFunctionType;
  typedef itk::Statistics::MembershipFunctionBase< MeasurementVectorType > BaseType;

  MembershipFunctionType::Pointer function = MembershipFunctionType::New();

  std::cout << function->GetNameOfClass() << std::endl;


  //set the distance metric type
  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType >  DistanceMetricType;
  typedef DistanceMetricType::MeasurementVectorSizeType MeasurementVectorSizeType;

  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();
  function->SetDistanceMetric( distanceMetric );

  if( function->GetDistanceMetric() != distanceMetric )
    {
    std::cerr << "Error in GetDistanceMetric() " << std::endl;
    return EXIT_FAILURE;
    }

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


  //Test if the distance computed is correct
  MembershipFunctionType::CentroidType origin;
  ::itk::Statistics::MeasurementVectorTraits::SetLength( origin, 3);
  origin[0] = 1.5;
  origin[1] = 2.3;
  origin[2] = 1.0;
  function->SetCentroid( origin );

  const double tolerance = 0.001;

  if( vcl_fabs( function->GetCentroid()[0] - origin[0]) > tolerance ||
      vcl_fabs( function->GetCentroid()[1] - origin[1]) > tolerance ||
      vcl_fabs( function->GetCentroid()[2] - origin[2]) > tolerance )
    {
    std::cerr << "Error in GetCentroid() method" << std::endl;
    return EXIT_FAILURE;
    }

  MeasurementVectorType measurement;
  ::itk::Statistics::MeasurementVectorTraits::SetLength( measurement, 3);
  measurement[0] = 2.5;
  measurement[1] = 3.3;
  measurement[2] = 4.0;

  double trueValue = 3.31662;
  double distanceComputed = function->Evaluate( measurement );

  if( vcl_fabs( distanceComputed - trueValue) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue
              << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the Clone method.
  MembershipFunctionType::Pointer clonedFunction = function->Clone();

  return EXIT_SUCCESS;
}
