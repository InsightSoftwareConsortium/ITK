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

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkBSplineKernelFunction.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkIndexRange.h"
#include "itkPointSet.h"
#include "itkVector.h"
#include "itkGTest.h"

#include <random>
#include <vector>

namespace
{
// Evaluates the B-spline kernel of the given order with the kernel classes that the filter uses.
double
EvaluateKernel(unsigned int order, double u)
{
  switch (order)
  {
    case 1:
      return itk::BSplineKernelFunction<1>::New()->Evaluate(u);
    case 2:
      return itk::BSplineKernelFunction<2>::New()->Evaluate(u);
    case 3:
      return itk::BSplineKernelFunction<3>::New()->Evaluate(u);
    default:
    {
      auto kernel = itk::CoxDeBoorBSplineKernelFunction<3>::New();
      kernel->SetSplineOrder(order);
      return kernel->Evaluate(u);
    }
  }
}

// Fits one level with one work unit, and compares the control point lattice with a reference that evaluates the
// kernel for each neighbor and each axis, with the expressions and summation order of a direct implementation.
template <unsigned int VDimension, unsigned int VComponents>
void
CheckFitAgainstReference(const std::vector<unsigned int> & orders, const std::vector<bool> & closed)
{
  using DataType = itk::Vector<float, VComponents>;
  using PointSetType = itk::PointSet<DataType, VDimension>;
  using ImageType = itk::Image<DataType, VDimension>;
  using FilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, ImageType>;
  using RealType = typename FilterType::RealType;
  using LatticeType = typename FilterType::PointDataImageType;
  using WeightLatticeType = itk::Image<RealType, VDimension>;

  const unsigned int              imageSize = VDimension < 4 ? 20 : 8;
  typename ImageType::SizeType    size;
  typename ImageType::PointType   origin;
  typename ImageType::SpacingType spacing;
  typename FilterType::ArrayType  splineOrder;
  typename FilterType::ArrayType  numberOfControlPoints;
  typename FilterType::ArrayType  closeDimension;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    size[i] = imageSize;
    origin[i] = -3.5 + i;
    spacing[i] = 0.75 + 0.5 * i;
    splineOrder[i] = orders[i];
    numberOfControlPoints[i] = orders[i] + 3;
    closeDimension[i] = closed[i] ? 1 : 0;
  }

  std::mt19937                          generator(VDimension * 100 + VComponents);
  std::uniform_real_distribution<float> unit(0.0F, 1.0F);
  auto                                  pointSet = PointSetType::New();
  auto                                  weights = FilterType::WeightsContainerType::New();
  for (unsigned int n = 0; n < 300; ++n)
  {
    typename PointSetType::PointType point;
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      point[i] = origin[i] + unit(generator) * (imageSize - 1) * spacing[i];
    }
    DataType value;
    for (unsigned int c = 0; c < VComponents; ++c)
    {
      value[c] = 10.0F * unit(generator) - 5.0F;
    }
    pointSet->SetPoint(n, point);
    pointSet->SetPointData(n, value);
    weights->InsertElement(n, 0.25F + unit(generator));
  }

  auto filter = FilterType::New();
  filter->SetSize(size);
  filter->SetOrigin(origin);
  filter->SetSpacing(spacing);
  filter->SetSplineOrder(splineOrder);
  filter->SetNumberOfControlPoints(numberOfControlPoints);
  filter->SetNumberOfLevels(itk::MakeFilled<typename FilterType::ArrayType>(1));
  filter->SetCloseDimension(closeDimension);
  filter->SetGenerateOutputImage(false);
  filter->SetNumberOfWorkUnits(1);
  filter->SetInput(pointSet);
  filter->SetPointWeights(weights);
  filter->Update();
  const LatticeType * phi = filter->GetPhiLattice();

  // Reference: the same fit with one kernel evaluation per neighbor and axis.
  typename LatticeType::SizeType latticeSize;
  typename LatticeType::SizeType neighborhoodSize;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    latticeSize[i] = closed[i] ? numberOfControlPoints[i] - orders[i] : numberOfControlPoints[i];
    neighborhoodSize[i] = orders[i] + 1;
  }
  auto delta = LatticeType::New();
  delta->SetRegions(latticeSize);
  delta->AllocateInitialized();
  auto omega = WeightLatticeType::New();
  omega->SetRegions(latticeSize);
  omega->AllocateInitialized();

  const RealType bsplineEpsilon = filter->GetBSplineEpsilon();
  for (unsigned int n = 0; n < pointSet->GetNumberOfPoints(); ++n)
  {
    typename PointSetType::PointType point;
    pointSet->GetPoint(n, &point);
    itk::FixedArray<RealType, VDimension> p;
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      const unsigned int totalNumberOfSpans = numberOfControlPoints[i] - orders[i];
      const RealType     r =
        static_cast<RealType>(numberOfControlPoints[i] - orders[i]) / (static_cast<RealType>(size[i] - 1) * spacing[i]);
      const RealType epsilon = r * spacing[i] * bsplineEpsilon;
      p[i] = (point[i] - origin[i]) * r;
      if (itk::Math::Absolute(p[i] - static_cast<RealType>(totalNumberOfSpans)) <= epsilon)
      {
        p[i] = static_cast<RealType>(totalNumberOfSpans) - epsilon;
      }
      if (p[i] < RealType{} && itk::Math::Absolute(p[i]) <= epsilon)
      {
        p[i] = RealType{};
      }
    }

    const typename LatticeType::RegionType neighborhood(neighborhoodSize);
    std::vector<RealType>                  B;
    RealType                               w2Sum = 0.0;
    for (const auto & index : itk::ImageRegionIndexRange<VDimension>(neighborhood))
    {
      RealType b = 1.0;
      for (unsigned int i = 0; i < VDimension; ++i)
      {
        const RealType u = static_cast<RealType>(p[i] - static_cast<unsigned int>(p[i]) - index[i]) +
                           0.5 * static_cast<RealType>(orders[i] - 1);
        b *= EvaluateKernel(orders[i], u);
      }
      B.push_back(b);
      w2Sum += b * b;
    }

    const RealType wc = weights->GetElement(n);
    DataType       value{};
    pointSet->GetPointData(n, &value);
    unsigned int k = 0;
    for (const auto & index : itk::ImageRegionIndexRange<VDimension>(neighborhood))
    {
      typename LatticeType::IndexType latticeIndex = index;
      for (unsigned int i = 0; i < VDimension; ++i)
      {
        latticeIndex[i] += static_cast<unsigned int>(p[i]);
        if (closed[i])
        {
          latticeIndex[i] %= latticeSize[i];
        }
      }
      const RealType t = B[k++];
      omega->SetPixel(latticeIndex, omega->GetPixel(latticeIndex) + wc * t * t);
      DataType data = value;
      data *= (t * t * t * wc / w2Sum);
      delta->SetPixel(latticeIndex, delta->GetPixel(latticeIndex) + data);
    }
  }

  ASSERT_EQ(phi->GetBufferedRegion().GetSize(), latticeSize);
  unsigned int numberOfNonzeroControlPoints = 0;
  for (itk::ImageRegionConstIteratorWithIndex<LatticeType> it(phi, phi->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    DataType       expected{};
    const RealType w = omega->GetPixel(it.GetIndex());
    if (itk::Math::NotAlmostEquals(w, RealType{}))
    {
      expected = delta->GetPixel(it.GetIndex()) / w;
      for (unsigned int c = 0; c < VComponents; ++c)
      {
        if (itk::Math::isnan(expected[c]) || itk::Math::isinf(expected[c]))
        {
          expected[c] = 0;
        }
      }
      ++numberOfNonzeroControlPoints;
    }
    for (unsigned int c = 0; c < VComponents; ++c)
    {
      ASSERT_EQ(it.Get()[c], expected[c]) << "control point " << it.GetIndex() << ", component " << c;
    }
  }
  EXPECT_GT(numberOfNonzeroControlPoints, 0U);
}
} // namespace

TEST(BSplineScatteredDataPointSetToImageFilter, FitMatchesReferenceForEachSplineOrder)
{
  for (unsigned int order = 1; order <= 5; ++order)
  {
    SCOPED_TRACE(order);
    CheckFitAgainstReference<2, 1>({ order, order }, { false, false });
    CheckFitAgainstReference<3, 3>({ order, order, order }, { false, false, false });
  }
}

TEST(BSplineScatteredDataPointSetToImageFilter, FitMatchesReferenceForMixedOrders)
{
  CheckFitAgainstReference<3, 1>({ 1, 3, 5 }, { false, false, false });
  CheckFitAgainstReference<3, 3>({ 5, 2, 4 }, { false, false, false });
  CheckFitAgainstReference<4, 2>({ 2, 3, 1, 4 }, { false, false, false, false });
}

TEST(BSplineScatteredDataPointSetToImageFilter, FitMatchesReferenceForClosedDimensions)
{
  CheckFitAgainstReference<1, 1>({ 3 }, { true });
  CheckFitAgainstReference<2, 1>({ 3, 3 }, { true, true });
  CheckFitAgainstReference<2, 3>({ 3, 3 }, { true, false });
  CheckFitAgainstReference<3, 1>({ 1, 3, 5 }, { true, false, true });
  CheckFitAgainstReference<4, 1>({ 2, 3, 1, 4 }, { false, true, false, true });
}

TEST(BSplineScatteredDataPointSetToImageFilter, FitMatchesReferenceInOneDimension)
{
  for (unsigned int order = 1; order <= 5; ++order)
  {
    SCOPED_TRACE(order);
    CheckFitAgainstReference<1, 1>({ order }, { false });
    CheckFitAgainstReference<1, 3>({ order }, { order % 2 == 0 });
  }
}
