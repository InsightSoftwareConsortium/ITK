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

namespace itk
{

template <typename TInput,   // LevelSetImageType
          typename TFeature, // FeatureImageType
          typename TSharedData>
class RegionBasedLevelSetFunctionTestHelper : public RegionBasedLevelSetFunction<TInput, TFeature, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionBasedLevelSetFunctionTestHelper);

  /** Standard class type aliases. */
  using Self = RegionBasedLevelSetFunctionTestHelper;
  using Superclass = RegionBasedLevelSetFunction<TInput, TFeature, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(RegionBasedLevelSetFunctionTestHelper, RegionBasedLevelSetFunction);

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

protected:
  RegionBasedLevelSetFunctionTestHelper() = default;
  ~RegionBasedLevelSetFunctionTestHelper() override = default;
};

template <unsigned int VDimension>
class RegionBasedLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class type aliases. */
  using Self = RegionBasedLevelSetFunctionSharedDataHelper;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(RegionBasedLevelSetFunctionSharedDataHelper, DataObject);

  unsigned long m_FunctionCount;

  using IndexType = Index<VDimension>;

  struct SingleData
  {
    unsigned int m_WeightedNumberOfPixelsInsideLevelSet;
    IndexType
    GetFeatureIndex(const IndexType & indx)
    {
      return indx;
    }
  };

  SingleData * m_LevelSetDataPointerVector[19];
};

} // namespace itk

int
itkRegionBasedLevelSetFunctionTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;

  using DataHelperType = itk::RegionBasedLevelSetFunctionSharedDataHelper<Dimension>;

  using RegionBasedLevelSetFunctionType =
    itk::RegionBasedLevelSetFunctionTestHelper<ImageType, FeatureImageType, DataHelperType>;

  auto function = RegionBasedLevelSetFunctionType::New();
  if (function.IsNull())
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
