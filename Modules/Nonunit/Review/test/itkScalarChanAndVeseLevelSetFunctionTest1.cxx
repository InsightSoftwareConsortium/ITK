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

#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk
{

template <typename TInput,   // LevelSetImageType
          typename TFeature, // FeatureImageType
          typename TSharedData>
class ScalarChanAndVeseLevelSetFunctionTestHelper
  : public ScalarChanAndVeseLevelSetFunction<TInput, TFeature, TSharedData>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarChanAndVeseLevelSetFunctionTestHelper);

  /** Standard class type aliases. */
  using Self = ScalarChanAndVeseLevelSetFunctionTestHelper;
  using Superclass = ScalarChanAndVeseLevelSetFunction<TInput, TFeature, TSharedData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarChanAndVeseLevelSetFunctionTestHelper, ScalarChanAndVeseLevelSetFunction);

  using typename Superclass::ScalarValueType;
  using typename Superclass::FeaturePixelType;
  using typename Superclass::FeatureIndexType;


  virtual ScalarValueType
  computeInternalTerm(const FeaturePixelType &, const FeatureIndexType &, const unsigned int &)
  {
    return ScalarValueType(0);
  }

  virtual ScalarValueType
  computeExternalTerm(const FeaturePixelType &, const FeatureIndexType &, const unsigned int &)
  {
    return ScalarValueType(0);
  }

  virtual void
  computeOverlapParameters(const FeatureIndexType, unsigned int &, unsigned int &)
  {}

  void
  ComputeParameters() override
  {}

protected:
  ScalarChanAndVeseLevelSetFunctionTestHelper() = default;
  ~ScalarChanAndVeseLevelSetFunctionTestHelper() override = default;
};

template <unsigned int VDimension>
class ScalarChanAndVeseLevelSetFunctionSharedDataHelper : public DataObject
{
public:
  /** Standard class type aliases. */
  using Self = ScalarChanAndVeseLevelSetFunctionSharedDataHelper;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ScalarChanAndVeseLevelSetFunctionSharedDataHelper, DataObject);

  unsigned long m_FunctionCount;

  using IndexType = Index<VDimension>;
  using ListPixelType = std::list<unsigned int>;
  using ImageType = Image<ListPixelType, VDimension>;

  typename ImageType::Pointer m_NearestNeighborListImage;

  using PixelType = double;
  using InputImageType = Image<PixelType, VDimension>;

  struct SingleData
  {
    IndexType
    GetIndex(const IndexType & globalIndex)
    {
      return globalIndex;
    }

    IndexType
    GetFeatureIndex(const IndexType & indx)
    {
      return indx;
    }

    typename InputImageType::Pointer m_HeavisideFunctionOfLevelSetImage;

    double m_WeightedNumberOfPixelsInsideLevelSet;
    double m_WeightedSumOfPixelValuesInsideLevelSet;
    double m_ForegroundConstantValues;

    double m_WeightedNumberOfPixelsOutsideLevelSet;
    double m_WeightedSumOfPixelValuesOutsideLevelSet;
    double m_BackgroundConstantValues;
  };

  SingleData * m_LevelSetDataPointerVector[19];
};

} // namespace itk

int
itkScalarChanAndVeseLevelSetFunctionTest1(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;

  using DataHelperType = itk::ScalarChanAndVeseLevelSetFunctionSharedDataHelper<Dimension>;


  using ChanAndVeseLevelSetFunctionType =
    itk::ScalarChanAndVeseLevelSetFunctionTestHelper<ImageType, FeatureImageType, DataHelperType>;

  auto function = ChanAndVeseLevelSetFunctionType::New();

  std::cout << "GetNameOfClass() = " << function->GetNameOfClass() << std::endl;
  function->Print(std::cout);

  return EXIT_SUCCESS;
}
