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

#include "itkVectorConfidenceConnectedImageFilter.h"
#include "itkVectorImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkGTest.h"

#include <cmath>

namespace
{
using ImageType = itk::VectorImage<float, 2>;
using OutputImageType = itk::Image<unsigned char, 2>;
using FilterType = itk::VectorConfidenceConnectedImageFilter<ImageType, OutputImageType>;
} // namespace

// An outlier seed converges to an empty pass; the next re-estimation must stop instead of
// dividing by zero samples and flooding the image with NaN statistics (issue #6575, B33).
TEST(VectorConfidenceConnectedImageFilter, EmptyPassStopsReestimation)
{
  auto image = ImageType::New();
  image->SetVectorLength(2);
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(16) });
  image->Allocate();
  itk::VariableLengthVector<float> background(2);
  background.Fill(100);
  image->FillBuffer(background);
  const ImageType::IndexType       seedIndex{ { 8, 8 } };
  itk::VariableLengthVector<float> seedValue(2);
  seedValue.Fill(0);
  image->SetPixel(seedIndex, seedValue);

  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetInitialNeighborhoodRadius(1);
  filter->AddSeed(seedIndex);
  filter->SetMultiplier(2.5);
  filter->SetNumberOfIterations(4);
  filter->SetReplaceValue(255);
  EXPECT_NO_THROW(filter->Update());

  const OutputImageType * output = filter->GetOutput();
  itk::SizeValueType      segmentedCount = 0;
  for (itk::ImageRegionConstIterator<OutputImageType> it(output, output->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    segmentedCount += (it.Get() == 255) ? 1 : 0;
  }
  EXPECT_EQ(segmentedCount, 0u);

  const FilterType::MeanVectorType & mean = filter->GetMean();
  for (unsigned int i = 0; i < mean.size(); ++i)
  {
    EXPECT_FALSE(std::isnan(mean[i])) << "mean[" << i << ']';
  }
  const FilterType::CovarianceMatrixType & covariance = filter->GetCovariance();
  for (unsigned int r = 0; r < covariance.rows(); ++r)
  {
    for (unsigned int c = 0; c < covariance.cols(); ++c)
    {
      EXPECT_FALSE(std::isnan(covariance[r][c])) << "covariance(" << r << ',' << c << ')';
    }
  }
}
