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
#include "itkHistogram.h"
#include "itkMahalanobisDistanceMetric.h"

namespace itk {
namespace Statistics {
template < typename TSample >
class MyCovarianceSampleFilter : public CovarianceSampleFilter< TSample >
{
public:
  typedef MyCovarianceSampleFilter                Self;
  typedef CovarianceSampleFilter<TSample>         Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
  typedef TSample                                 SampleType;

  itkNewMacro(Self);

  //method to invoke MakeOutput with index value different
  //from one or zero. This is to check if an exception will be
  // thrown

  void CreateInvalidOutput()
    {
    unsigned int index=3;
    Superclass::MakeOutput( index );
    }
  unsigned int GetMeasurementVectorSize() const
    {
    return this->Superclass::GetMeasurementVectorSize();
    }

private:
  MyCovarianceSampleFilter() {}
  ~MyCovarianceSampleFilter() ITK_OVERRIDE {}
};
}
}

int itkCovarianceSampleFilterTest3(int, char* [] )
{
  std::cout << "CovarianceSampleFilter test \n \n";

  typedef double                      MeasurementType;
  const unsigned int                  MeasurementVectorSize = 3;

  typedef itk::Statistics::Histogram< MeasurementType,
          itk::Statistics::DenseFrequencyContainer2 > HistogramType;

  typedef HistogramType    SampleType;

  HistogramType::Pointer histogram = HistogramType::New();

  HistogramType::SizeType                 size( MeasurementVectorSize );
  HistogramType::MeasurementVectorType    lowerBound( MeasurementVectorSize );
  HistogramType::MeasurementVectorType    upperBound( MeasurementVectorSize );

  size.Fill(50);
  lowerBound.Fill(-350);
  upperBound.Fill(450);

  histogram->SetMeasurementVectorSize( MeasurementVectorSize );
  histogram->Initialize( size, lowerBound, upperBound );
  histogram->SetToZero();

  typedef itk::Statistics::MahalanobisDistanceMetric<
    HistogramType::MeasurementVectorType >                    MembershipFunctionType;

  MembershipFunctionType::Pointer memberFunction = MembershipFunctionType::New();


  typedef MembershipFunctionType::MeanVectorType            MeanVectorType;
  typedef MembershipFunctionType::CovarianceMatrixType      CovarianceMatrixType;

  MeanVectorType mean( MeasurementVectorSize );
  CovarianceMatrixType covariance( MeasurementVectorSize, MeasurementVectorSize );

  mean[0] = 50;
  mean[1] = 52;
  mean[2] = 51;

  covariance.set_identity();
  covariance[0][0] = 10000.0;
  covariance[1][1] = 8000.0;
  covariance[2][2] = 6000.0;


  for( unsigned int i=0; i < MeasurementVectorSize; i++ )
    {
    for( unsigned int j=i; j < MeasurementVectorSize; j++ )
      {
      covariance[j][i] = covariance[i][j];
      }
    }

  std::cout << "Initial Mean = " << std::endl << mean << std::endl;
  std::cout << "Initial Covariance = " << std::endl << covariance << std::endl;

  memberFunction->SetMean( mean );
  memberFunction->SetCovariance( covariance );

  HistogramType::Iterator itr = histogram->Begin();
  HistogramType::Iterator end = histogram->End();

  typedef HistogramType::AbsoluteFrequencyType  AbsoluteFrequencyType;

  while( itr != end )
    {
    const double MahalanobisDistance =
      memberFunction->Evaluate( itr.GetMeasurementVector() );

    const double MahalanobisDistance2 = MahalanobisDistance * MahalanobisDistance;

    AbsoluteFrequencyType frequency = (AbsoluteFrequencyType) std::floor( 1e5 * std::exp( -0.5 * MahalanobisDistance2 ) );

    itr.SetFrequency( frequency );
    ++itr;
    }


  typedef itk::Statistics::MyCovarianceSampleFilter< SampleType > FilterType;

  FilterType::Pointer filter = FilterType::New();


  //test if exception is thrown if a derived class tries to create
  // an invalid output
  try
    {
    filter->CreateInvalidOutput();
    std::cerr << "Exception should have been thrown: " << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Expected Exception caught: " << excp << std::endl;
    }

  filter->ResetPipeline();
  filter->SetInput( histogram );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  const FilterType::MatrixDecoratedType * decorator = filter->GetCovarianceMatrixOutput();
  FilterType::MatrixType    covarianceOutput  = decorator->Get();

  FilterType::MeasurementVectorRealType meanOutput = filter->GetMean();

  std::cout << "Mean: "              << meanOutput << std::endl;
  std::cout << "Covariance Matrix: " << covarianceOutput << std::endl;

  std::cout << "GetMeasurementVectorSize = " << filter->GetMeasurementVectorSize() << std::endl;

  double epsilon = 1;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    if ( std::fabs( meanOutput[i] - mean[i] ) > epsilon )
      {
      std::cerr << "The computed mean value is incorrect" << std::endl;
      std::cerr << "computed mean = " << meanOutput << std::endl;
      std::cerr << "expected mean = " << mean << std::endl;
      return EXIT_FAILURE;
      }
    }

  epsilon = 35;

  for ( unsigned int i = 0; i < MeasurementVectorSize; i++ )
    {
    for ( unsigned int j = 0; j < MeasurementVectorSize; j++ )
      {
      if ( std::fabs( covariance[i][j] - covarianceOutput[i][j] ) > epsilon )
        {
        std::cerr << "Computed covariance matrix value is incorrrect:"
                  << i << "," << j << "=" << covariance[i][j]
                  << "," << covarianceOutput[i][j] << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
