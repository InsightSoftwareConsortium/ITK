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

#include "itkScalarRegionBasedLevelSetFunction.h"

namespace itk
{

template <typename TInput,   // LevelSetImageType
          typename TFeature, // FeatureImageType
          typename TSharedData>
class ScalarRegionBasedLevelSetFunctionTestHelper
  : public ScalarRegionBasedLevelSetFunction<TInput, TFeature, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarRegionBasedLevelSetFunctionTestHelper);

  /** Standard class type aliases. */
  using Self = ScalarRegionBasedLevelSetFunctionTestHelper;
  using Superclass = ScalarRegionBasedLevelSetFunction<TInput, TFeature, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarRegionBasedLevelSetFunctionTestHelper, ScalarRegionBasedLevelSetFunction);

  using typename Superclass::ScalarValueType;
  using typename Superclass::FeaturePixelType;
  using typename Superclass::FeatureIndexType;

  ScalarValueType
  ComputeInternalTerm(const FeaturePixelType &, const FeatureIndexType &) override
  {
    return ScalarValueType(0);
  }

  ScalarValueType
  ComputeExternalTerm(const FeaturePixelType &, const FeatureIndexType &) override
  {
    return ScalarValueType(0);
  }

  ScalarValueType
  ComputeOverlapParameters(const FeatureIndexType &, ScalarValueType &) override
  {
    return ScalarValueType(0);
  }

  void
  ComputeParameters() override
  {}

  void
  UpdateSharedDataParameters() override
  {}

  void
  UpdateSharedDataInsideParameters(const unsigned int &, const FeaturePixelType &, const ScalarValueType &) override
  {}

  void
  UpdateSharedDataOutsideParameters(const unsigned int &, const FeaturePixelType &, const ScalarValueType &) override
  {}

protected:
  ScalarRegionBasedLevelSetFunctionTestHelper() = default;
  ~ScalarRegionBasedLevelSetFunctionTestHelper() override = default;
};

template <unsigned int VDimension>
class ScalarRegionBasedLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class type aliases. */
  using Self = ScalarRegionBasedLevelSetFunctionSharedDataHelper;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarRegionBasedLevelSetFunctionSharedDataHelper, DataObject);

  unsigned long m_FunctionCount;

  using IndexType = Index<VDimension>;
  using ListPixelType = std::list<unsigned int>;

  using ImageType = Image<ListPixelType, VDimension>;
  typename ImageType::Pointer m_NearestNeighborListImage;

  using PixelType = double;
  using InputImageType = Image<PixelType, VDimension>;

  struct SingleData
  {
    typename InputImageType::Pointer m_HeavisideFunctionOfLevelSetImage;
    int                              m_WeightedNumberOfPixelsInsideLevelSet;
    int                              m_WeightedSumOfPixelValuesInsideLevelSet;
    int                              m_ForegroundConstantValues;

    int m_WeightedNumberOfPixelsOutsideLevelSet;
    int m_WeightedSumOfPixelValuesOutsideLevelSet;
    int m_BackgroundConstantValues;

    IndexType
    GetFeatureIndex(const IndexType & indx)
    {
      return indx;
    }

    IndexType
    GetIndex(const IndexType & globalIndex)
    {
      return globalIndex;
    }
  };

  SingleData * m_LevelSetDataPointerVector[19];
};

} // namespace itk

int
itkScalarRegionBasedLevelSetFunctionTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;

  using DataHelperType = itk::ScalarRegionBasedLevelSetFunctionSharedDataHelper<Dimension>;

  using RegionBasedLevelSetFunctionType =
    itk::ScalarRegionBasedLevelSetFunctionTestHelper<ImageType, FeatureImageType, DataHelperType>;

  auto function = RegionBasedLevelSetFunctionType::New();
  if (function.IsNull())
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
