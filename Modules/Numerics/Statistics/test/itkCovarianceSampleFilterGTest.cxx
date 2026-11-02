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
#include "itkCovarianceSampleFilter.h"
#include "itkImageRegionIterator.h"
#include "itkGTest.h"

TEST(CovarianceSampleFilter, ConvertedLegacyTest)
{
  std::cout << "CovarianceSampleFilter Test \n \n";

  // Now generate an image
  enum
  {
    MeasurementVectorSize = 3
  };
  using MeasurementType = float;

  using MeasurementVectorType = itk::FixedArray<MeasurementType, MeasurementVectorSize>;
  using ImageType = itk::Image<MeasurementVectorType, 3>;
  using MaskImageType = itk::Image<unsigned char, 3>;

  auto                          image = ImageType::New();
  constexpr ImageType::SizeType size{ 5, 5, 5 };
  ImageType::RegionType         region{ size };


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
  using CovarianceSampleFilterType = itk::Statistics::CovarianceSampleFilter<ListSampleType>;

  auto covarianceFilter = CovarianceSampleFilterType::New();

  std::cout << "GetNameOfClass() = " << covarianceFilter->GetNameOfClass() << std::endl;

  // Invoke update before adding an input. An exception should be
  EXPECT_THROW(covarianceFilter->Update(), itk::ExceptionObject);

  covarianceFilter->ResetPipeline();

  EXPECT_EQ(covarianceFilter->GetInput(), nullptr);

  covarianceFilter->SetInput(sampleGeneratingFilter->GetOutput());
  ASSERT_NO_THROW(covarianceFilter->Update());

  covarianceFilter->Print(std::cout);

  constexpr double epsilon{ 1e-6 };

  // CHECK THE RESULTS
  const CovarianceSampleFilterType::MeasurementVectorDecoratedType * meanDecorator = covarianceFilter->GetMeanOutput();

  CovarianceSampleFilterType::MeasurementVectorRealType mean = meanDecorator->Get();
  std::cout << "Mean:   " << mean << std::endl;
  CovarianceSampleFilterType::MeasurementVectorRealType mean2 = covarianceFilter->GetMean();

  EXPECT_NEAR(mean[0], mean2[0], epsilon);
  EXPECT_NEAR(mean[1], mean2[1], epsilon);
  EXPECT_NEAR(mean[2], mean2[2], epsilon);


  const CovarianceSampleFilterType::MatrixDecoratedType * decorator = covarianceFilter->GetCovarianceMatrixOutput();
  const CovarianceSampleFilterType::MatrixType            covarianceMatrix = decorator->Get();

  std::cout << "Covariance matrix:   " << covarianceMatrix << std::endl;


  using MeanSampleFilterType = itk::Statistics::MeanSampleFilter<ListSampleType>;
  auto meanFilter = MeanSampleFilterType::New();
  meanFilter->SetInput(sampleGeneratingFilter->GetOutput());

  ASSERT_NO_THROW(meanFilter->Update());

  MeanSampleFilterType::MeasurementVectorRealType meanCalculatedUsingMeanSampleFilter = meanFilter->GetMean();

  EXPECT_NEAR(meanCalculatedUsingMeanSampleFilter[0], mean[0], epsilon);
  EXPECT_NEAR(meanCalculatedUsingMeanSampleFilter[1], mean[1], epsilon);
  EXPECT_NEAR(meanCalculatedUsingMeanSampleFilter[2], mean[2], epsilon);
}
