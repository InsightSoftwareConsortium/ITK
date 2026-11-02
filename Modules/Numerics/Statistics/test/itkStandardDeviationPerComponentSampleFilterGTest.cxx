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

#include "itkImageToListSampleFilter.h"
#include "itkStandardDeviationPerComponentSampleFilter.h"
#include "itkImageRegionIterator.h"
#include "itkCovarianceSampleFilter.h"
#include "itkGTest.h"

TEST(StandardDeviationPerComponentSampleFilter, ConvertedLegacyTest)
{
  std::cout << "StandardDeviationPerComponentSampleFilter Test \n \n";

  // Now generate an image
  enum
  {
    MeasurementVectorSize = 3
  };
  using MeasurementType = float;

  using MeasurementVectorType = itk::FixedArray<MeasurementType, MeasurementVectorSize>;
  using ImageType = itk::Image<MeasurementVectorType, MeasurementVectorSize>;
  using MaskImageType = itk::Image<unsigned char, MeasurementVectorSize>;

  auto image = ImageType::New();

  ImageType::SizeType   size{ 5, 5, 5 };
  ImageType::RegionType region{ size };


  image->SetBufferedRegion(region);
  image->Allocate();

  using ImageIterator = itk::ImageRegionIterator<ImageType>;
  ImageIterator iter(image, region);

  unsigned int          count = 0;
  MeasurementVectorType temp{};

  // fill the image
  while (!iter.IsAtEnd())
  {
    temp[0] = count;
    temp[1] = 2 * count;
    temp[2] = 3 * count;
    iter.Set(temp);
    ++iter;
    ++count;
  }

  // creates an ImageToListSampleAdaptor object
  using ImageToListSampleFilterType = itk::Statistics::ImageToListSampleFilter<ImageType, MaskImageType>;

  auto sampleGeneratingFilter = ImageToListSampleFilterType::New();

  sampleGeneratingFilter->SetInput(image);

  ASSERT_NO_THROW(sampleGeneratingFilter->Update());

  using ListSampleType = ImageToListSampleFilterType::ListSampleType;
  using StandardDeviationPerComponentSampleFilterType =
    itk::Statistics::StandardDeviationPerComponentSampleFilter<ListSampleType>;

  const StandardDeviationPerComponentSampleFilterType::Pointer standardDeviationFilter =
    StandardDeviationPerComponentSampleFilterType::New();

  std::cout << standardDeviationFilter->GetNameOfClass() << std::endl;

  // Invoke update before adding an input. An exception should be
  EXPECT_THROW(standardDeviationFilter->Update(), itk::ExceptionObject)
    << "Exception should have been thrown since Update() is invoked without setting an input ";

  standardDeviationFilter->ResetPipeline();

  EXPECT_EQ(standardDeviationFilter->GetInput(), nullptr)
    << "GetInput() should return nullptr if the input has not been set";

  standardDeviationFilter->SetInput(sampleGeneratingFilter->GetOutput());
  ASSERT_NO_THROW(standardDeviationFilter->Update());

  standardDeviationFilter->Print(std::cout);

  constexpr double epsilon{ 1e-6 };

  // CHECK THE RESULTS
  using MeasurementVectorRealType = StandardDeviationPerComponentSampleFilterType::MeasurementVectorRealType;
  using MeasurementVectorRealDecoratedType =
    StandardDeviationPerComponentSampleFilterType::MeasurementVectorRealDecoratedType;

  const MeasurementVectorRealDecoratedType * standardDeviationDecorator =
    standardDeviationFilter->GetStandardDeviationPerComponentOutput();

  const MeasurementVectorRealDecoratedType * meanDecorator = standardDeviationFilter->GetMeanPerComponentOutput();

  MeasurementVectorRealType standardDeviation = standardDeviationDecorator->Get();
  std::cout << "Standard deviation:   " << standardDeviation << std::endl;

  MeasurementVectorRealType mean = meanDecorator->Get();
  std::cout << "Mean :   " << mean << std::endl;

  MeasurementVectorRealType standardDeviation2 = standardDeviationFilter->GetStandardDeviationPerComponent();

  EXPECT_NEAR(standardDeviation[0], standardDeviation2[0], epsilon)
    << "Standard Deviation value retrieved using Get() and the decorator are not the same";
  EXPECT_NEAR(standardDeviation[1], standardDeviation2[1], epsilon)
    << "Standard Deviation value retrieved using Get() and the decorator are not the same";
  EXPECT_NEAR(standardDeviation[2], standardDeviation2[2], epsilon)
    << "Standard Deviation value retrieved using Get() and the decorator are not the same";


  using CovarianceSampleFilterType = itk::Statistics::CovarianceSampleFilter<ListSampleType>;
  auto covarianceFilter = CovarianceSampleFilterType::New();
  covarianceFilter->SetInput(sampleGeneratingFilter->GetOutput());

  ASSERT_NO_THROW(covarianceFilter->Update());

  CovarianceSampleFilterType::MeasurementVectorRealType meanCalculatedUsingCovarianceSampleFilter =
    covarianceFilter->GetMean();

  EXPECT_NEAR(meanCalculatedUsingCovarianceSampleFilter[0], mean[0], epsilon)
    << "Mean calculated using the CovarianceSampleFilter is different from the one calculated using the "
       "StandardDeviationPerComponentSampleFilter ";
  EXPECT_NEAR(meanCalculatedUsingCovarianceSampleFilter[1], mean[1], epsilon)
    << "Mean calculated using the CovarianceSampleFilter is different from the one calculated using the "
       "StandardDeviationPerComponentSampleFilter ";
  EXPECT_NEAR(meanCalculatedUsingCovarianceSampleFilter[2], mean[2], epsilon)
    << "Mean calculated using the CovarianceSampleFilter is different from the one calculated using the "
       "StandardDeviationPerComponentSampleFilter ";

  CovarianceSampleFilterType::MatrixType covarianceCalculatedUsingCovarianceSampleFilter =
    covarianceFilter->GetCovarianceMatrix();

  for (unsigned int k = 0; k < MeasurementVectorSize; ++k)
  {
    const double variance = covarianceCalculatedUsingCovarianceSampleFilter(k, k);
    const double standardDeviationValue = std::sqrt(variance);

    EXPECT_NEAR(standardDeviationValue, standardDeviation[k], epsilon)
      << "Standard deviation calculated using the CovarianceSampleFilter"
      << " (as the square root of the diagonal) is different from "
      << " the one calculated using the StandardDeviationPerComponentSampleFilter ";
  }
}
