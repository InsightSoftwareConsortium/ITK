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

#include <iostream>

#include "itkMersenneTwisterRandomVariateGenerator.h"

#include "itkStatisticsImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkStatisticsImageFilterTest(int argc, char * argv[])
{
  std::cout << "itkStatisticsImageFilterTest  [numberOfStreamDivisions]" << std::endl;

  int status = 0;


  unsigned int numberOfStreamDivisions = 1;

  if (argc > 1)
  {
    numberOfStreamDivisions = std::max(std::stoi(argv[1]), 1);
  }

  using FloatImage = itk::Image<int, 3>;

  itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed(987);

  auto                   image = FloatImage::New();
  FloatImage::RegionType region;
  FloatImage::SizeType   size;
  size.Fill(64);
  FloatImage::IndexType index;
  index.Fill(0);

  region.SetIndex(index);
  region.SetSize(size);

  // first try a constant image
  float fillValue = -100.0;
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(static_cast<FloatImage::PixelType>(fillValue));

  float sum = fillValue * static_cast<float>(region.GetNumberOfPixels());
  float sumOfSquares = std::pow(fillValue, 2.0) * static_cast<float>(region.GetNumberOfPixels());

  using FilterType = itk::StatisticsImageFilter<FloatImage>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, StatisticsImageFilter, ImageSink);


  itk::SimpleFilterWatcher filterWatch(filter);

  filter->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  ITK_TEST_SET_GET_VALUE(numberOfStreamDivisions, filter->GetNumberOfStreamDivisions());

  filter->SetInput(image);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  if (itk::Math::NotAlmostEquals(filter->GetMinimum(), fillValue))
  {
    std::cerr << "GetMinimum failed! Got " << filter->GetMinimum() << " but expected " << fillValue << std::endl;
    status++;
  }
  if (itk::Math::NotAlmostEquals(filter->GetMaximum(), fillValue))
  {
    std::cerr << "GetMaximum failed! Got " << filter->GetMaximum() << " but expected " << fillValue << std::endl;
    status++;
  }
  if (itk::Math::NotAlmostEquals(filter->GetSum(), sum))
  {
    std::cerr << "GetSum failed! Got " << filter->GetSum() << " but expected " << sum << std::endl;
    status++;
  }
  if (itk::Math::NotAlmostEquals(filter->GetSumOfSquares(), sumOfSquares))
  {
    std::cerr << "GetSumOfSquares failed! Got " << filter->GetSumOfSquares() << " but expected " << sumOfSquares
              << std::endl;
    status++;
  }

  if (itk::Math::NotAlmostEquals(filter->GetMean(), fillValue))
  {
    std::cerr << "GetMean failed! Got " << filter->GetMean() << " but expected " << fillValue << std::endl;
    status++;
  }
  if (itk::Math::NotAlmostEquals(filter->GetVariance(), 0.0))
  {
    std::cerr << "GetVariance failed! Got " << filter->GetVariance() << " but expected " << 0.0 << std::endl;
    status++;
  }


  // Now generate a real image

  using SourceType = itk::RandomImageSource<FloatImage>;
  auto source = SourceType::New();

  FloatImage::SizeValueType randomSize[3] = { 17, 8, 241 };

  source->SetSize(randomSize);
  float minValue = -100.0;
  float maxValue = 1000.0;

  source->SetMin(static_cast<FloatImage::PixelType>(minValue));
  source->SetMax(static_cast<FloatImage::PixelType>(maxValue));

  filter->SetInput(source->GetOutput());
  filter->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->UpdateLargestPossibleRegion());

  double expectedSigma = std::sqrt((maxValue - minValue) * (maxValue - minValue) / 12.0);
  double epsilon = (maxValue - minValue) * .001;

  if (itk::Math::abs(filter->GetSigma() - expectedSigma) > epsilon)
  {
    std::cerr << "GetSigma failed! Got " << filter->GetSigma() << " but expected " << expectedSigma << std::endl;
  }

  // Now generate an image with a known mean and variance
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer rvgen =
    itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();
  double knownMean = 12.0;
  double knownVariance = 10.0;

  using DoubleImage = itk::Image<double, 3>;
  auto                    dImage = DoubleImage::New();
  DoubleImage::SizeType   dsize;
  DoubleImage::IndexType  dindex;
  DoubleImage::RegionType dregion;
  dsize.Fill(50);
  dindex.Fill(0);
  dregion.SetSize(dsize);
  dregion.SetIndex(dindex);
  dImage->SetRegions(dregion);
  dImage->Allocate();
  itk::ImageRegionIterator<DoubleImage> it(dImage, dregion);
  while (!it.IsAtEnd())
  {
    it.Set(rvgen->GetNormalVariate(knownMean, knownVariance));
    ++it;
  }
  using DFilterType = itk::StatisticsImageFilter<DoubleImage>;
  auto dfilter = DFilterType::New();
  dfilter->SetInput(dImage);
  dfilter->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  ITK_TRY_EXPECT_NO_EXCEPTION(dfilter->UpdateLargestPossibleRegion());
  double testMean = dfilter->GetMean();
  double testVariance = dfilter->GetVariance();
  double diff = itk::Math::abs(testMean - knownMean);
  if ((diff != 0.0 && knownMean != 0.0) && diff / itk::Math::abs(knownMean) > .01)
  {
    std::cout << "Expected mean is " << knownMean << ", computed mean is " << testMean << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Expected mean is " << knownMean << ", computed mean is " << testMean << std::endl;
  diff = itk::Math::abs(testVariance - knownVariance);
  if ((diff != 0.0 && knownVariance != 0.0) && diff / itk::Math::abs(knownVariance) > .1)
  {
    std::cout << "Expected variance is " << knownVariance << ", computed variance is " << testVariance << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Expected variance is " << knownVariance << ", computed variance is " << testVariance << std::endl;
  return status;
}
