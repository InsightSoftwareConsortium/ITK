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

#include "itkRegionBasedLevelSetFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkDataObject.h"
#include "itkGTest.h"

namespace itk
{

template <typename TInput, typename TOutput>
class ConstantDerivativeHeavisideStepFunction : public HeavisideStepFunctionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConstantDerivativeHeavisideStepFunction);

  using Self = ConstantDerivativeHeavisideStepFunction;
  using Superclass = HeavisideStepFunctionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(ConstantDerivativeHeavisideStepFunction);

  using typename Superclass::InputType;
  using typename Superclass::OutputType;

  OutputType
  Evaluate(const InputType &) const override
  {
    return OutputType{};
  }

  OutputType
  EvaluateDerivative(const InputType &) const override
  {
    return m_DerivativeValue;
  }

  void
  SetDerivativeValue(OutputType value)
  {
    m_DerivativeValue = value;
  }

protected:
  ConstantDerivativeHeavisideStepFunction() = default;
  ~ConstantDerivativeHeavisideStepFunction() override = default;

private:
  OutputType m_DerivativeValue{};
};

class RegionBasedLevelSetReinitTestSharedData : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionBasedLevelSetReinitTestSharedData);

  using Self = RegionBasedLevelSetReinitTestSharedData;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(RegionBasedLevelSetReinitTestSharedData);

  struct SingleData
  {
    double m_WeightedNumberOfPixelsInsideLevelSet{};

    template <typename TIndex>
    TIndex
    GetFeatureIndex(const TIndex & index) const
    {
      return index;
    }
  };

  unsigned long m_FunctionCount{ 1 };
  SingleData    m_Data;
  SingleData *  m_LevelSetDataPointerVector[1]{ &m_Data };

protected:
  RegionBasedLevelSetReinitTestSharedData() = default;
  ~RegionBasedLevelSetReinitTestSharedData() override = default;
};

template <typename TInput, typename TFeature, typename TSharedData>
class RegionBasedLevelSetReinitTestFunction : public RegionBasedLevelSetFunction<TInput, TFeature, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionBasedLevelSetReinitTestFunction);

  using Self = RegionBasedLevelSetReinitTestFunction;
  using Superclass = RegionBasedLevelSetFunction<TInput, TFeature, TSharedData>;
  using Pointer = SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(RegionBasedLevelSetReinitTestFunction);

  using typename Superclass::ScalarValueType;
  using typename Superclass::FeaturePixelType;
  using typename Superclass::FeatureIndexType;

  ScalarValueType
  ComputeInternalTerm(const FeaturePixelType &, const FeatureIndexType &) override
  {
    return ScalarValueType{};
  }

  ScalarValueType
  ComputeExternalTerm(const FeaturePixelType &, const FeatureIndexType &) override
  {
    return ScalarValueType{};
  }

  ScalarValueType
  ComputeOverlapParameters(const FeatureIndexType &, ScalarValueType & pr) override
  {
    pr = ScalarValueType{};
    return ScalarValueType{};
  }

  void
  ComputeParameters() override
  {}

  void
  UpdateSharedDataParameters() override
  {}

protected:
  RegionBasedLevelSetReinitTestFunction() = default;
  ~RegionBasedLevelSetReinitTestFunction() override = default;
};

} // namespace itk

namespace
{
using ImageType = itk::Image<double, 2>;
using FunctionType =
  itk::RegionBasedLevelSetReinitTestFunction<ImageType, ImageType, itk::RegionBasedLevelSetReinitTestSharedData>;

// phi = x^2 + y^2 gives curvature 1 and Laplacian 4 at index (3,2), unit spacing.
ImageType::Pointer
MakeParaboloidImage()
{
  const ImageType::RegionType region(ImageType::SizeType::Filled(5));
  auto                        image = ImageType::New();
  image->SetRegions(region);
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, region); !it.IsAtEnd(); ++it)
  {
    const auto   index = it.GetIndex();
    const double x = index[0] - 2.0;
    const double y = index[1] - 2.0;
    it.Set(x * x + y * y);
  }
  return image;
}

double
ComputeReinitializationOnlyUpdate(double curvatureWeight, double heavisideDerivative)
{
  const auto phiImage = MakeParaboloidImage();

  auto featureImage = ImageType::New();
  featureImage->SetRegions(phiImage->GetLargestPossibleRegion());
  featureImage->Allocate();
  featureImage->FillBuffer(0.0);

  auto domainFunction = itk::ConstantDerivativeHeavisideStepFunction<double, double>::New();
  domainFunction->SetDerivativeValue(heavisideDerivative);

  auto sharedData = itk::RegionBasedLevelSetReinitTestSharedData::New();

  auto function = FunctionType::New();
  function->SetFeatureImage(featureImage);
  function->SetDomainFunction(domainFunction);
  function->SetSharedData(sharedData);
  function->SetFunctionId(0);
  function->SetCurvatureWeight(curvatureWeight);
  function->SetReinitializationSmoothingWeight(1.0);

  const auto radius = itk::MakeFilled<FunctionType::RadiusType>(1);
  function->Initialize(radius);

  FunctionType::NeighborhoodType neighborhood(radius, phiImage, phiImage->GetLargestPossibleRegion());
  neighborhood.SetLocation(ImageType::IndexType{ { 3, 2 } });

  void *       globalData = function->GetGlobalDataPointer();
  const double update = function->ComputeUpdate(neighborhood, globalData);
  function->ReleaseGlobalDataPointer(globalData);

  return update;
}

} // namespace

TEST(RegionBasedLevelSetFunction, ReinitializationTermSubtractsCurvatureWhenCurvatureWeightIsZero)
{
  EXPECT_DOUBLE_EQ(ComputeReinitializationOnlyUpdate(0.0, 2.0), 3.0);
}

TEST(RegionBasedLevelSetFunction, ReinitializationTermSubtractsCurvatureWhenDerivativeIsZero)
{
  EXPECT_DOUBLE_EQ(ComputeReinitializationOnlyUpdate(1.0, 0.0), 3.0);
}
