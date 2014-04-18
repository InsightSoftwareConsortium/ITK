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

#include "itkCovarianceSampleFilter.h"
#include "itkListSample.h"

int itkCovarianceSampleFilterTest2(int, char* [] )
{
  std::cout << "CovarianceSampleFilter test \n \n";

  const unsigned int                  MeasurementVectorSize = 3;
  const unsigned int                  numberOfMeasurementVectors = 3;
  unsigned int                        counter;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >             MeasurementVectorType;
  typedef itk::Statistics::ListSample<
    MeasurementVectorType >                    SampleType;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( MeasurementVectorSize );

  MeasurementVectorType               measure;

  //reset counter
  counter = 0;

  while ( counter < numberOfMeasurementVectors )
    {
    for( unsigned int i=0; i<MeasurementVectorSize; i++)
      {
      measure[i] = counter;
      }
    sample->PushBack( measure );
    counter++;
    }

  typedef itk::Statistics::CovarianceSampleFilter< SampleType >
    FilterType;

  typedef FilterType::MatrixType          CovarianceMatrixType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( sample );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  std::cout << "Mean: " << filter->GetMean() << std::endl;

  const FilterType::MatrixDecoratedType * decorator = filter->GetCovarianceMatrixOutput();
  FilterType::MatrixType    covarianceOutput  = decorator->Get();

  std::cout << "Covariance Matrix: " << covarianceOutput << std::endl;

  const FilterType::MeasurementVectorDecoratedType * meanDecorator  = filter->GetMeanOutput();
  FilterType::MeasurementVectorRealType    mean = meanDecorator->Get();

  //Check the results

  typedef FilterType::MeasurementVectorRealType   MeasurementVectorRealType;
  MeasurementVectorRealType  meanExpected;

  itk::NumericTraits<MeasurementVectorRealType>::SetLength( meanExpected, MeasurementVectorSize );

  meanExpected.Fill( 1.0 );

  const double epsilon = 1e-4;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::fabs( meanExpected[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  CovarianceMatrixType  matrixExpected( MeasurementVectorSize, MeasurementVectorSize );

  matrixExpected[0][0] = 1.0;
  matrixExpected[0][1] = 1.0;
  matrixExpected[0][2] = 1.0;

  matrixExpected[1][0] = 1.0;
  matrixExpected[1][1] = 1.0;
  matrixExpected[1][2] = 1.0;

  matrixExpected[2][0] = 1.0;
  matrixExpected[2][1] = 1.0;
  matrixExpected[2][2] = 1.0;

  for( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if( std::fabs( matrixExpected[i][j] - covarianceOutput[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrect" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  // use orthogonal meausrment vectors
  SampleType::Pointer sample2 = SampleType::New();

  sample2->SetMeasurementVectorSize( MeasurementVectorSize );

  MeasurementVectorType               measure2;

  //reset counter
  counter = 0;

  while ( counter < numberOfMeasurementVectors )
    {
    for( unsigned int i=0; i<MeasurementVectorSize; i++)
      {
      if ( counter == i )
        {
        measure2[i] = 1.0;
        }
      else
        {
        measure2[i] = 0.0;
        }
      }
    sample2->PushBack( measure2 );
    counter++;
    }

  filter->SetInput( sample2 );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  std::cout << "Mean: " << filter->GetMean() << std::endl;
  std::cout << "Covariance Matrix: " << filter->GetCovarianceMatrix() << std::endl;

  mean = filter->GetMean();
  CovarianceMatrixType matrix = filter->GetCovarianceMatrix();

  //Check the results

  MeasurementVectorRealType  meanExpected2;
  itk::NumericTraits<MeasurementVectorRealType>::SetLength( meanExpected2, MeasurementVectorSize );

  meanExpected2.Fill( 0.333333 );

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::fabs( meanExpected2[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  CovarianceMatrixType  matrixExpected2( MeasurementVectorSize, MeasurementVectorSize );

  matrixExpected2[0][0] = 0.33333;
  matrixExpected2[0][1] = -0.16667;
  matrixExpected2[0][2] = -0.16667;

  matrixExpected2[1][0] = -0.16667;
  matrixExpected2[1][1] = 0.33333;
  matrixExpected2[1][2] = -0.16667;

  matrixExpected2[2][0] = -0.16667;
  matrixExpected2[2][1] = -0.16667;
  matrixExpected2[2][2] = 0.333333;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if ( std::fabs( matrixExpected2[i][j] - matrix[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrect" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  SampleType::Pointer sample3 = SampleType::New();

  sample2->SetMeasurementVectorSize( MeasurementVectorSize );

  MeasurementVectorType               measure3;

  measure3[0] =  4.00;
  measure3[1] =  2.00;
  measure3[2] =  0.60;
  sample3->PushBack( measure3 );

  measure3[0] =  4.20;
  measure3[1] =  2.10;
  measure3[2] =  0.59;
  sample3->PushBack( measure3 );

  measure3[0] =  3.90;
  measure3[1] =  2.00;
  measure3[2] =  0.58;
  sample3->PushBack( measure3 );

  measure3[0] =  4.30;
  measure3[1] =  2.10;
  measure3[2] =  0.62;
  sample3->PushBack( measure3 );

  measure3[0] =  4.10;
  measure3[1] =  2.20;
  measure3[2] =  0.63;
  sample3->PushBack( measure3 );


  filter->SetInput( sample3 );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }


  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: "              << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  //Check the results

  MeasurementVectorRealType  meanExpected3;
  itk::NumericTraits<MeasurementVectorRealType>::SetLength( meanExpected3, MeasurementVectorSize );

  meanExpected3[0] = 4.10;
  meanExpected3[1] = 2.08;
  meanExpected3[2] = 0.604;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::fabs( meanExpected3[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

 CovarianceMatrixType  matrixExpected3( MeasurementVectorSize, MeasurementVectorSize );

 matrixExpected3[0][0] = 0.025;
 matrixExpected3[0][1] = 0.0075;
 matrixExpected3[0][2] = 0.00175;

 matrixExpected3[1][0] = 0.0075;
 matrixExpected3[1][1] = 0.0070;
 matrixExpected3[1][2] = 0.00135;

 matrixExpected3[2][0] = 0.00175;
 matrixExpected3[2][1] = 0.00135;
 matrixExpected3[2][2] = 0.00043;

 for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
  {
  for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
    if ( std::fabs( matrixExpected3[i][j] - matrix[i][j] ) > epsilon )
      {
      std::cerr << "Computed covariance matrix value is incorrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
