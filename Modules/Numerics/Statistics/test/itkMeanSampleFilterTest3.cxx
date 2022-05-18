/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkMeanSampleFilter.h"
#include "itkHistogram.h"
#include "itkMahalanobisDistanceMetric.h"

int
itkMeanSampleFilterTest3(int, char *[])
{
  std::cout << "CovarianceSampleFilter test \n \n";

  using MeasurementType = double;
  constexpr unsigned int MeasurementVectorSize = 3;

  using HistogramType = itk::Statistics::Histogram<MeasurementType, itk::Statistics::DenseFrequencyContainer2>;

  using SampleType = HistogramType;

  auto histogram = HistogramType::New();

  HistogramType::SizeType              size(MeasurementVectorSize);
  HistogramType::MeasurementVectorType lowerBound(MeasurementVectorSize);
  HistogramType::MeasurementVectorType upperBound(MeasurementVectorSize);

  size.Fill(50);
  lowerBound.Fill(-350);
  upperBound.Fill(450);

  histogram->SetMeasurementVectorSize(MeasurementVectorSize);
  histogram->Initialize(size, lowerBound, upperBound);
  histogram->SetToZero();

  using MembershipFunctionType = itk::Statistics::MahalanobisDistanceMetric<HistogramType::MeasurementVectorType>;

  auto memberFunction = MembershipFunctionType::New();


  using MeanVectorType = MembershipFunctionType::MeanVectorType;
  using CovarianceMatrixType = MembershipFunctionType::CovarianceMatrixType;

  MeanVectorType       mean(MeasurementVectorSize);
  CovarianceMatrixType covariance(MeasurementVectorSize, MeasurementVectorSize);

  mean[0] = 50;
  mean[1] = 52;
  mean[2] = 51;

  covariance.set_identity();
  covariance[0][0] = 10000.0;
  covariance[1][1] = 8000.0;
  covariance[2][2] = 6000.0;


  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    for (unsigned int j = i; j < MeasurementVectorSize; ++j)
    {
      covariance[j][i] = covariance[i][j];
    }
  }

  std::cout << "Initial Mean = " << std::endl << mean << std::endl;
  std::cout << "Initial Covariance = " << std::endl << covariance << std::endl;

  memberFunction->SetMean(mean);
  memberFunction->SetCovariance(covariance);

  HistogramType::Iterator itr = histogram->Begin();
  HistogramType::Iterator end = histogram->End();

  using AbsoluteFrequencyType = HistogramType::AbsoluteFrequencyType;

  while (itr != end)
  {
    const double MahalanobisDistance = memberFunction->Evaluate(itr.GetMeasurementVector());

    const double MahalanobisDistance2 = MahalanobisDistance * MahalanobisDistance;

    auto frequency = (AbsoluteFrequencyType)std::floor(1e5 * std::exp(-0.5 * MahalanobisDistance2));

    itr.SetFrequency(frequency);
    ++itr;
  }


  using FilterType = itk::Statistics::MeanSampleFilter<SampleType>;

  auto filter = FilterType::New();

  filter->SetInput(histogram);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  FilterType::MeasurementVectorRealType meanOutput = filter->GetMean();

  std::cout << "Mean: " << meanOutput << std::endl;

  std::cout << "GetMeasurementVectorSize = " << filter->GetMeasurementVectorSize() << std::endl;

  double epsilon = 1;

  for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
  {
    if (itk::Math::abs(meanOutput[i] - mean[i]) > epsilon)
    {
      std::cerr << "The computed mean value is incorrect" << std::endl;
      std::cerr << "computed mean = " << meanOutput << std::endl;
      std::cerr << "expected mean = " << mean << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
