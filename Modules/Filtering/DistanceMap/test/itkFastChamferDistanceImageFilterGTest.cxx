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

#include "itkFastChamferDistanceImageFilter.h"
#include "itkMath.h"
#include <gtest/gtest.h>

namespace
{
// simple signed distance function
template <typename TPoint>
double
SimpleSignedDistance(const TPoint & p)
{
  auto             center = itk::MakeFilled<TPoint>(16);
  constexpr double radius{ 10 };

  double accum = 0.0;
  for (unsigned int j = 0; j < TPoint::PointDimension; ++j)
  {
    accum += static_cast<double>(itk::Math::sqr(p[j] - center[j]));
  }
  accum = std::sqrt(accum);
  if (itk::Math::abs(accum - radius) > 1)
  {
    if ((accum - radius) > 0)
    {
      return radius;
    }

    return -radius;
  }
  else
  {
    return accum - radius;
  }
}


template <unsigned int VDimension>
void
FastChamferDistanceImageFilterTest(unsigned int iPositive, unsigned int iNegative, unsigned int iOther)
{
  // Test ITK Chamfer Distance Image Filter
  // Compute the distance map of a 32^d image

  SCOPED_TRACE("Dimension = " + std::to_string(VDimension));

  using PixelType = float;

  using ImageType = itk::Image<PixelType, VDimension>;
  using PointType = itk::Point<double, VDimension>;

  auto                                 size = ImageType::SizeType::Filled(32);
  const typename ImageType::IndexType  index{};
  const typename ImageType::RegionType region{ index, size };

  auto inputImage = ImageType::New();
  inputImage->SetRegions(region);
  inputImage->Allocate();

  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  IteratorType it(inputImage, region);

  // Set the image to 0
  while (!it.IsAtEnd())
  {
    PointType point;
    inputImage->TransformIndexToPhysicalPoint(it.GetIndex(), point);
    it.Set(SimpleSignedDistance(point));
    ++it;
  }

  /* Create Fast Chamfer Distance filter */
  using ChamferFilterType = itk::FastChamferDistanceImageFilter<ImageType, ImageType>;
  auto filter = ChamferFilterType::New();

  filter->SetInput(inputImage);

  const typename ImageType::Pointer outputImage = filter->GetOutput();

  EXPECT_NO_THROW(filter->Update());

  filter->Print(std::cout);

  // Create NarrowBand
  using NarrowBandType = typename ChamferFilterType::NarrowBandType;

  auto band = NarrowBandType::New();
  band->SetTotalRadius(4);
  band->SetInnerRadius(2);
  filter->SetMaximumDistance(5);
  std::cout << "Band initial size: " << band->Size() << std::endl;
  filter->SetNarrowBand(band);
  filter->Update();

  std::cout << "Band size: " << band->Size() << std::endl;

  // Loop through the band
  auto       itNB = band->Begin();
  const auto itNBend = band->End();

  //  BandNodeType *tmp;
  unsigned int innerpositive = 0;
  unsigned int innernegative = 0;
  unsigned int otherpoints = 0;
  while (itNB != itNBend)
  {
    if (itNB->m_NodeState == 3)
    {
      innerpositive++;
    }
    else if (itNB->m_NodeState == 2)
    {
      innernegative++;
    }
    else
    {
      otherpoints++;
    }
    ++itNB;
  }

  EXPECT_EQ(innerpositive, iPositive);
  EXPECT_EQ(innernegative, iNegative);
  EXPECT_EQ(otherpoints, iOther);

  // Exercising filter methods
  float inweights[VDimension];
  for (unsigned int dim = 0; dim < VDimension; ++dim)
  {
    if (dim == 0)
    {
      inweights[dim] = 0.926f;
    }
    else if (dim == 1)
    {
      inweights[dim] = 1.34f;
    }
    else
    {
      inweights[dim] = 0.f;
    }
  }
  filter->SetWeights(inweights);
  const float * outweights = filter->GetWeights().GetDataPointer();

  std::cout << "outweights = " << outweights << std::endl;

  EXPECT_FLOAT_EQ(filter->GetMaximumDistance(), 5);

  /* For debugging write the result
  using WriterType = itk::ImageFileWriter< ImageType >;
  auto writer = WriterType::New();

  writer->SetFileName("chamferoutput.mhd");
  writer->SetInput(filter->GetOutput());
  writer->Update();
  */
}

} // namespace


TEST(FastChamferDistanceImageFilter, Test)
{
  FastChamferDistanceImageFilterTest<1>(4, 6, 8);
  FastChamferDistanceImageFilterTest<2>(144, 124, 256);
  FastChamferDistanceImageFilterTest<3>(3068, 2066, 5520);
  FastChamferDistanceImageFilterTest<4>(49472, 28928, 93136);
}
