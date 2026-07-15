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

#include "itkMultiphaseSparseFiniteDifferenceImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"

#include "itkGTest.h"

namespace itk
{

template <typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell>
class MultiphaseSparseFiniteDifferenceImageFilterGTestHelper
  : public MultiphaseSparseFiniteDifferenceImageFilter<TInputImage,
                                                       TFeatureImage,
                                                       TOutputImage,
                                                       TFiniteDifferenceFunction,
                                                       TIdCell>
{
public:
  using Self = MultiphaseSparseFiniteDifferenceImageFilterGTestHelper;
  using Superclass = MultiphaseSparseFiniteDifferenceImageFilter<TInputImage,
                                                                 TFeatureImage,
                                                                 TOutputImage,
                                                                 TFiniteDifferenceFunction,
                                                                 TIdCell>;
  using Pointer = SmartPointer<Self>;

  itkNewMacro(Self);

  using typename Superclass::TimeStepType;
  using typename Superclass::ValueType;
  using Superclass::InitializeBackgroundConstants;

  void
  AllocateUpdateBuffer() override
  {}

  void
  ApplyUpdate(TimeStepType itkNotUsed(dt)) override
  {}

  TimeStepType
  CalculateChange() override
  {
    return TimeStepType(1.0);
  }

  void
  CopyInputToOutput() override
  {}

  ValueType
  GetBackgroundValueForTest() const
  {
    return this->m_BackgroundValue;
  }
};

} // namespace itk

TEST(MultiphaseSparseFiniteDifferenceImageFilter, BackgroundConstantUsesNonpositiveMinSeed)
{
  constexpr unsigned int Dimension = 3;
  using LevelSetImageType = itk::Image<double, Dimension>;
  using FeatureImageType = itk::Image<float, Dimension>;
  using OutputImageType = itk::Image<unsigned char, Dimension>;

  using DataHelperType = itk::ScalarChanAndVeseLevelSetFunctionData<LevelSetImageType, FeatureImageType>;
  using SharedDataHelperType =
    itk::ConstrainedRegionBasedLevelSetFunctionSharedData<LevelSetImageType, FeatureImageType, DataHelperType>;
  using RegionBasedLevelSetFunctionType =
    itk::ScalarChanAndVeseLevelSetFunction<LevelSetImageType, FeatureImageType, SharedDataHelperType>;

  using IdCellType = unsigned long;
  using FilterType = itk::MultiphaseSparseFiniteDifferenceImageFilterGTestHelper<LevelSetImageType,
                                                                                 FeatureImageType,
                                                                                 OutputImageType,
                                                                                 RegionBasedLevelSetFunctionType,
                                                                                 IdCellType>;

  auto filter = FilterType::New();
  filter->SetFunctionCount(1);

  auto                        levelSet = LevelSetImageType::New();
  LevelSetImageType::SizeType size;
  size.Fill(4);
  const LevelSetImageType::RegionType region(size);
  levelSet->SetRegions(region);

  LevelSetImageType::SpacingType tinySpacing;
  tinySpacing.Fill(1e-40);
  levelSet->SetSpacing(tinySpacing);
  levelSet->Allocate();
  levelSet->FillBuffer(0.0);

  filter->SetLevelSet(0, levelSet);
  filter->InitializeBackgroundConstants();

  EXPECT_LT(filter->GetBackgroundValueForTest(), 1e-38);
}
