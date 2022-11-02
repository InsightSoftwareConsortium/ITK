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

int
itkStandardDeviationPerComponentSampleFilterTest(int, char *[])
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

  auto                  image = ImageType::New();
  ImageType::RegionType region;
  ImageType::SizeType   size;
  ImageType::IndexType  index;
  index.Fill(0);
  size.Fill(5);
  region.SetIndex(index);
  region.SetSize(size);


  image->SetBufferedRegion(region);
  image->Allocate();

  using ImageIterator = itk::ImageRegionIterator<ImageType>;
  ImageIterator iter(image, region);

  unsigned int          count = 0;
  MeasurementVectorType temp;
  temp.Fill(0);

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

  try
  {
    sampleGeneratingFilter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using ListSampleType = ImageToListSampleFilterType::ListSampleType;
  using StandardDeviationPerComponentSampleFilterType =
    itk::Statistics::StandardDeviationPerComponentSampleFilter<ListSampleType>;

  StandardDeviationPerComponentSampleFilterType::Pointer standardDeviationFilter =
    StandardDeviationPerComponentSampleFilterType::New();

  std::cout << standardDeviationFilter->GetNameOfClass() << std::endl;

  // Invoke update before adding an input. An exception should be
  try
  {
    standardDeviationFilter->Update();
    std::cerr << "Exception should have been thrown since Update() is invoked without setting an input " << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  standardDeviationFilter->ResetPipeline();

  if (standardDeviationFilter->GetInput() != nullptr)
  {
    std::cerr << "GetInput() should return nullptr if the input has not been set" << std::endl;
    return EXIT_FAILURE;
  }

  standardDeviationFilter->SetInput(sampleGeneratingFilter->GetOutput());
  try
  {
    standardDeviationFilter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  standardDeviationFilter->Print(std::cout);

  const double epsilon = 1e-6;

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

  if ((itk::Math::abs(standardDeviation[0] - standardDeviation2[0]) > epsilon) ||
      (itk::Math::abs(standardDeviation[1] - standardDeviation2[1]) > epsilon) ||
      (itk::Math::abs(standardDeviation[2] - standardDeviation2[2]) > epsilon))
  {
    std::cerr << "Standard Deviation value retrieved using Get() and the decorator are not the same:: "
              << standardDeviation << "," << standardDeviation2 << std::endl;
    return EXIT_FAILURE;
  }


  using CovarianceSampleFilterType = itk::Statistics::CovarianceSampleFilter<ListSampleType>;
  auto covarianceFilter = CovarianceSampleFilterType::New();
  covarianceFilter->SetInput(sampleGeneratingFilter->GetOutput());

  try
  {
    covarianceFilter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught: " << excp << std::endl;
  }

  CovarianceSampleFilterType::MeasurementVectorRealType meanCalculatedUsingCovarianceSampleFilter =
    covarianceFilter->GetMean();

  if ((itk::Math::abs(meanCalculatedUsingCovarianceSampleFilter[0] - mean[0]) > epsilon) ||
      (itk::Math::abs(meanCalculatedUsingCovarianceSampleFilter[1] - mean[1]) > epsilon) ||
      (itk::Math::abs(meanCalculatedUsingCovarianceSampleFilter[2] - mean[2]) > epsilon))
  {
    std::cerr << "Mean calculated using the CovarianceSampleFilter is different from the one calculated using the "
                 "StandardDeviationPerComponentSampleFilter "
              << std::endl;
    return EXIT_FAILURE;
  }

  CovarianceSampleFilterType::MatrixType covarianceCalculatedUsingCovarianceSampleFilter =
    covarianceFilter->GetCovarianceMatrix();

  for (unsigned int k = 0; k < MeasurementVectorSize; ++k)
  {
    const double variance = covarianceCalculatedUsingCovarianceSampleFilter(k, k);
    const double standardDeviationValue = std::sqrt(variance);

    if ((itk::Math::abs(standardDeviationValue - standardDeviation[k]) > epsilon))
    {
      std::cerr << "Standard deviation calculated using the CovarianceSampleFilter";
      std::cerr << " (as the square root of the diagonal) is different from ";
      std::cerr << " the one calculated using the StandardDeviationPerComponentSampleFilter " << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
