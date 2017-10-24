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

#include "itkWeightedCovarianceSampleFilter.h"
#include "itkListSample.h"

const unsigned int                  MeasurementVectorSize = 3;
unsigned int                        counter = 0;

typedef itk::FixedArray<
  float, MeasurementVectorSize >             MeasurementVectorType;

namespace itk {
namespace Statistics {
template < typename TSample >
class MyWeightedCovarianceSampleFilter : public WeightedCovarianceSampleFilter< TSample >
{
public:
  typedef MyWeightedCovarianceSampleFilter           Self;

  typedef WeightedCovarianceSampleFilter<TSample>     Superclass;

  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>             ConstPointer;
  typedef TSample                              SampleType;

  itkNewMacro(Self);

  //method to invoke MakeOutput with index value different
  //from one or zero. This is to check if an exception will be
  // thrown
  void CreateInvalidOutput()
    {
    unsigned int index=3;
    Superclass::MakeOutput( index );
    }
};
}
}


class WeightedCovarianceTestFunction :
  public itk::FunctionBase< MeasurementVectorType, double >
{
public:

  /** Standard class typedefs. */
  typedef WeightedCovarianceTestFunction Self;

  typedef itk::FunctionBase< MeasurementVectorType, double > Superclass;

  typedef itk::SmartPointer<Self> Pointer;

  typedef itk::SmartPointer<const Self> ConstPointer;

  /** Standard macros. */
  itkTypeMacro(WeightedCovarianceTestFunction, FunctionBase);
  itkNewMacro(Self);

  /** Input type */
  typedef MeasurementVectorType InputType;

  /** Output type */
  typedef double OutputType;

  /**Evaluate at the specified input position */
  virtual OutputType Evaluate( const InputType & itkNotUsed( input ) ) const ITK_OVERRIDE
    {
    MeasurementVectorType measurements;
    // set the weight factor of the measurment
    // vector with valuev[2, 2] to 0.5.
    return 1.0;
    }

protected:
  WeightedCovarianceTestFunction() {}
  ~WeightedCovarianceTestFunction() ITK_OVERRIDE {}
}; // end of class


int itkWeightedCovarianceSampleFilterTest(int, char* [] )
{
  std::cout << "WeightedCovarianceSampleFilter test \n \n";

 typedef itk::Statistics::ListSample<
    MeasurementVectorType >                    SampleType;

  typedef itk::Statistics::MyWeightedCovarianceSampleFilter< SampleType > FilterType;

  typedef FilterType::MeasurementVectorRealType  MeasurementVectorRealType;
  typedef FilterType::MatrixType                 CovarianceMatrixType;

  FilterType::Pointer filter = FilterType::New();

  MeasurementVectorType               measure;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( MeasurementVectorSize );

  measure[0] =  4.00;
  measure[1] =  2.00;
  measure[2] =  0.60;
  sample->PushBack( measure );

  measure[0] =  4.20;
  measure[1] =  2.10;
  measure[2] =  0.59;
  sample->PushBack( measure );

  measure[0] =  3.90;
  measure[1] =  2.00;
  measure[2] =  0.58;
  sample->PushBack( measure );

  measure[0] =  4.30;
  measure[1] =  2.10;
  measure[2] =  0.62;
  sample->PushBack( measure );

  measure[0] =  4.10;
  measure[1] =  2.20;
  measure[2] =  0.63;
  sample->PushBack( measure );

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  //Invoke update before adding an input. An exception should be
  //thrown.
  try
    {
    filter->Update();
    std::cerr << "Exception should have been thrown since \
                    Update() is invoked without setting an input" << std::endl;
    return EXIT_FAILURE;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception caught: " << excp << std::endl;
    }

  if ( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should return ITK_NULLPTR if the input \
                     has not been set" << std::endl;
    return EXIT_FAILURE;
    }

  //test if exception is thrown if a derived class tries to create
  // an invalid output
  try
    {
    filter->CreateInvalidOutput();
    std::cerr << "Exception should have been thrown: " << std::endl;
    return EXIT_FAILURE;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception caught: " << excp << std::endl;
    }

  filter->ResetPipeline();

  // Run the filter with no weights
  filter->SetInput( sample );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception caught: " << excp << std::endl;
    }

  MeasurementVectorRealType  mean = filter->GetMean();
  CovarianceMatrixType matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: "              << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  //Check the results

  double epsilon = 1e-2;

  MeasurementVectorRealType  meanExpected33;
  itk::NumericTraits< MeasurementVectorRealType >::SetLength( meanExpected33, MeasurementVectorSize );
  meanExpected33[0] = 4.10;
  meanExpected33[1] = 2.08;
  meanExpected33[2] = 0.604;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::abs( meanExpected33[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  CovarianceMatrixType  matrixExpected33( MeasurementVectorSize, MeasurementVectorSize );

  matrixExpected33[0][0] = 0.025;
  matrixExpected33[0][1] = 0.0075;
  matrixExpected33[0][2] = 0.00175;

  matrixExpected33[1][0] = 0.0075;
  matrixExpected33[1][1] = 0.0070;
  matrixExpected33[1][2] = 0.00135;

  matrixExpected33[2][0] = 0.00175;
  matrixExpected33[2][1] = 0.00135;
  matrixExpected33[2][2] = 0.00043;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
  {
  for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
    if ( std::abs( matrixExpected33[i][j] - matrix[i][j] ) > epsilon )
      {
      std::cerr << "Computed covariance matrix value is incorrrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //Specify weight
  typedef FilterType::WeightArrayType  WeightArrayType;
  WeightArrayType weightArray(sample->Size());
  weightArray.Fill(1.0);

  filter->SetWeights( weightArray );

  // run with equal weights
  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    return EXIT_FAILURE;
    }


  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: "              << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  MeasurementVectorRealType  meanExpected3;

  itk::NumericTraits< MeasurementVectorRealType >::SetLength( meanExpected3, MeasurementVectorSize );
  meanExpected3[0] = 4.10;
  meanExpected3[1] = 2.08;
  meanExpected3[2] = 0.604;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::abs( meanExpected3[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  CovarianceMatrixType  matrixExpected( MeasurementVectorSize, MeasurementVectorSize );

  matrixExpected[0][0] = 0.025;
  matrixExpected[0][1] = 0.0075;
  matrixExpected[0][2] = 0.00175;

  matrixExpected[1][0] = 0.0075;
  matrixExpected[1][1] = 0.0070;
  matrixExpected[1][2] = 0.00135;

  matrixExpected[2][0] = 0.00175;
  matrixExpected[2][1] = 0.00135;
  matrixExpected[2][2] = 0.00043;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if ( std::abs( matrixExpected[i][j] - matrix[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrrect" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  filter->SetWeights( weightArray );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    return EXIT_FAILURE;
    }

  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: "              << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::abs( meanExpected3[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if ( std::abs( matrixExpected[i][j] - matrix[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrrect" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  //set  a constant 1.0 weight using a function
  WeightedCovarianceTestFunction::Pointer weightFunction = WeightedCovarianceTestFunction::New();
  filter->SetWeightingFunction( weightFunction.GetPointer() );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    return EXIT_FAILURE;
    }

  mean = filter->GetMean();
  matrix = filter->GetCovarianceMatrix();

  std::cout << "Mean: "              << mean << std::endl;
  std::cout << "Covariance Matrix: " << matrix << std::endl;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::abs( meanExpected3[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrrect" << std::endl;
      return EXIT_FAILURE;
      }
    }

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if ( std::abs( matrixExpected[i][j] - matrix[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrrect" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
