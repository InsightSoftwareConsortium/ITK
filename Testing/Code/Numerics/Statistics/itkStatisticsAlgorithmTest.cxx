/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkStatisticsAlgorithmTest.cxx
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

#include "itkArray.h"
#include "itkVariableLengthVector.h"
#include "itkListSample.h"
#include "itkStatisticsAlgorithm.h"

int itkStatisticsAlgorithmTest( int, char * [] )
{
  std::cout << "StatisticsAlgorithm Test \n \n";

  const unsigned int measurementVectorSize = 2;

  typedef itk::Array< float > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  SampleType::Pointer sample = SampleType::New();

  SampleType::MeasurementVectorType lower( measurementVectorSize );
  SampleType::MeasurementVectorType upper( measurementVectorSize );

  const SampleType * constSample = sample.GetPointer();

  // Testing the exception throwing for samples of measurement size = 0
  sample->SetMeasurementVectorSize( 0 );

  try
    {
    itk::Statistics::Algorithm::FindSampleBound(
      constSample,
      constSample->Begin(), constSample->End(),
      lower, upper
      );
    std::cerr << "Failure to throw expected exception when " << std::endl;
    std::cerr << " MeasurementVectorType() has been set to zero" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Got Expected exception" << std::endl;
    }

  // Now set the correct measurement vector size
  sample->SetMeasurementVectorSize( measurementVectorSize );

  // Testing the equivalent of an empty sample by passing
  // the Begin() iterator inlieu of the End() iterator.
  //

  try
    {
    itk::Statistics::Algorithm::FindSampleBound(
      constSample,
      constSample->Begin(), constSample->Begin(),
      lower, upper
      );
    std::cerr << "Failure to throw expected exception when " << std::endl;
    std::cerr << " attempting to compute bound of an empty sample list" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    }


  MeasurementVectorType measure( measurementVectorSize );

  MeasurementVectorType realUpper( measurementVectorSize );
  MeasurementVectorType realLower( measurementVectorSize );

  const unsigned int numberOfSamples = 25;

  realLower.Fill( 1000 );
  realUpper.Fill(    0 );

  // Force the first value not to be the min or max.
  measure[0] = 13;
  measure[1] = 39;

  sample->PushBack( measure );

  for( unsigned int i = 1; i < numberOfSamples; i++ )
    {
    float value = i + 3;
    measure[0] = value;
    measure[1] = value * value;
    sample->PushBack( measure );

    for(unsigned int j = 0; j < measurementVectorSize; j++ )
      {
      if( measure[j] < realLower[j] )
        {
        realLower[j] = measure[j];
        }
      if( measure[j] > realUpper[j] )
        {
        realUpper[j] = measure[j];
        }
      }
    }


  // Now testing the real algorithm
  try
    {
    itk::Statistics::Algorithm::FindSampleBound(
      constSample,
      constSample->Begin(), constSample->End(),
      lower, upper
      );
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    return EXIT_FAILURE;
    }


  const float epsilon = 1e-5;

  for(unsigned int j = 0; j < measurementVectorSize; j++ )
    {
    if( vnl_math_abs( lower[j] - realLower[j] ) > epsilon )
      {
      std::cerr << "FindSampleBound() failed" << std::endl;
      std::cerr << "Expected lower = " << realLower << std::endl;
      std::cerr << "Computed lower = " << lower << std::endl;
      return EXIT_FAILURE;
      }
    if( vnl_math_abs( upper[j] - realUpper[j] ) > epsilon )
      {
      std::cerr << "FindSampleBound() failed" << std::endl;
      std::cerr << "Expected upper = " << realUpper << std::endl;
      std::cerr << "Computed upper = " << upper << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
