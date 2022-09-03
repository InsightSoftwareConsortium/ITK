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

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkTestingMacros.h"

namespace itk
{

template <typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell>
class MultiphaseDenseFiniteDifferenceImageFilterTestHelper
  : public MultiphaseDenseFiniteDifferenceImageFilter<TInputImage,
                                                      TFeatureImage,
                                                      TOutputImage,
                                                      TFiniteDifferenceFunction,
                                                      TIdCell>
{
public:
  /** Standard class type aliases. */
  using Self = MultiphaseDenseFiniteDifferenceImageFilterTestHelper;
  using Superclass = MultiphaseDenseFiniteDifferenceImageFilter<TInputImage,
                                                                TFeatureImage,
                                                                TOutputImage,
                                                                TFiniteDifferenceFunction,
                                                                TIdCell>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MultiphaseDenseFiniteDifferenceImageFilterTestHelper, MultiphaseDenseFiniteDifferenceImageFilter);

  itkNewMacro(Self);

  void
  AllocateUpdateBuffer() override
  {}

  using typename Superclass::TimeStepType;

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
};

} // namespace itk

int
itkMultiphaseDenseFiniteDifferenceImageFilterTest(int, char *[])
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

  auto function = RegionBasedLevelSetFunctionType::New();
  if (function.IsNull())
  {
    return EXIT_FAILURE;
  }

  using IdCellType = unsigned long;

  using FilterType = itk::MultiphaseDenseFiniteDifferenceImageFilterTestHelper<LevelSetImageType,
                                                                               FeatureImageType,
                                                                               OutputImageType,
                                                                               RegionBasedLevelSetFunctionType,
                                                                               IdCellType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    filter, MultiphaseDenseFiniteDifferenceImageFilterTestHelper, MultiphaseDenseFiniteDifferenceImageFilter);


  unsigned int reinitializeCounter = 1;
  filter->SetReinitializeCounter(reinitializeCounter);
  ITK_TEST_SET_GET_VALUE(reinitializeCounter, filter->GetReinitializeCounter());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
