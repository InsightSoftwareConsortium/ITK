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

#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>

namespace
{
constexpr unsigned int Dimension = 3;
using FieldType = itk::Image<itk::Vector<double, Dimension>, Dimension>;
using FloatFieldType = itk::Image<itk::Vector<float, Dimension>, Dimension>;

// A smooth field large enough that the inversion runs for several iterations.
template <typename TField = FieldType>
typename TField::Pointer
MakeField()
{
  using ValueType = typename TField::PixelType::ValueType;
  auto field = TField::New();
  field->SetRegions(TField::SizeType::Filled(16));
  field->Allocate();
  for (itk::ImageRegionIteratorWithIndex<TField> it(field, field->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    const typename TField::IndexType & index = it.GetIndex();
    typename TField::PixelType         v;
    v[0] = static_cast<ValueType>(1.5 * std::sin(0.4 * index[0] + 0.2 * index[1]));
    v[1] = static_cast<ValueType>(-1.0 * std::cos(0.3 * index[1]) * std::sin(0.25 * index[2]));
    v[2] = static_cast<ValueType>(0.8 * std::sin(0.35 * index[2] - 0.1 * index[0]));
    it.Set(v);
  }
  return field;
}

template <typename TField>
typename itk::InvertDisplacementFieldImageFilter<TField>::Pointer
MakeInverter(const TField * field)
{
  auto inverter = itk::InvertDisplacementFieldImageFilter<TField>::New();
  inverter->SetInput(field);
  inverter->SetMaximumNumberOfIterations(10);
  inverter->SetMeanErrorToleranceThreshold(0.0);
  inverter->SetMaxErrorToleranceThreshold(0.0);
  return inverter;
}

template <typename TField>
void
ExpectSameField(const TField * expected, const TField * actual)
{
  ASSERT_EQ(expected->GetBufferedRegion(), actual->GetBufferedRegion());
  itk::ImageRegionConstIterator<TField> itE(expected, expected->GetBufferedRegion());
  itk::ImageRegionConstIterator<TField> itA(actual, actual->GetBufferedRegion());
  for (; !itE.IsAtEnd(); ++itE, ++itA)
  {
    ASSERT_EQ(itE.Get(), itA.Get()) << "at " << itE.GetIndex();
  }
}

// Largest displacement norm away from the border.
template <typename TField>
double
MaximumInteriorNorm(const TField * field)
{
  typename TField::RegionType interior = field->GetLargestPossibleRegion();
  interior.ShrinkByRadius(4);
  double maximum = 0.0;
  for (itk::ImageRegionConstIterator<TField> it(field, interior); !it.IsAtEnd(); ++it)
  {
    maximum = std::max(maximum, static_cast<double>(it.Get().GetNorm()));
  }
  return maximum;
}

// Largest displacement left after applying the inverse and then the field, away from the border.
template <typename TField>
double
MaximumInteriorCompositionError(const TField * field, const TField * inverse)
{
  auto composer = itk::ComposeDisplacementFieldsImageFilter<TField>::New();
  composer->SetDisplacementField(field);
  composer->SetWarpingField(inverse);
  composer->Update();
  return MaximumInteriorNorm(composer->GetOutput());
}

class GlobalReleaseDataFlagGuard
{
public:
  GlobalReleaseDataFlagGuard()
    : m_Previous(itk::DataObject::GetGlobalReleaseDataFlag())
  {
    itk::DataObject::SetGlobalReleaseDataFlag(true);
  }
  ~GlobalReleaseDataFlagGuard() { itk::DataObject::SetGlobalReleaseDataFlag(m_Previous); }

private:
  bool m_Previous;
};
} // namespace

// Applying the inverse and then the field leaves almost no displacement, away from the border.
TEST(InvertDisplacementFieldImageFilter, InverseUndoesTheField)
{
  const auto field = MakeField();
  auto       inverter = MakeInverter(field.GetPointer());
  inverter->Update();

  const double residual = MaximumInteriorCompositionError(field.GetPointer(), inverter->GetOutput());
  EXPECT_LT(residual, 0.05 * MaximumInteriorNorm(field.GetPointer())) << "residual " << residual;
}

// With GlobalReleaseDataFlag on, the internal composer releases its inputs after every iteration's Update().
TEST(InvertDisplacementFieldImageFilter, GlobalReleaseDataFlagGivesTheSameInverse)
{
  const auto field = MakeField();
  auto       reference = MakeInverter(field.GetPointer());
  reference->Update();

  // A separate input, because the flag also releases the input after released->Update().
  const auto                       releasedField = MakeField();
  const GlobalReleaseDataFlagGuard releaseData;
  auto                             released = MakeInverter(releasedField.GetPointer());
  released->Update();

  ASSERT_NO_FATAL_FAILURE(ExpectSameField(reference->GetOutput(), released->GetOutput()));
}

// A second Update() of the same filter, after its input changes, runs the inversion again from the start.
TEST(InvertDisplacementFieldImageFilter, RepeatedUpdateGivesTheSameInverse)
{
  const auto field = MakeField();
  auto       inverter = MakeInverter(field.GetPointer());
  inverter->Update();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(it.Get() * 0.5);
  }
  field->Modified();
  inverter->Update();

  auto fresh = MakeInverter(field.GetPointer());
  fresh->Update();

  ASSERT_NO_FATAL_FAILURE(ExpectSameField(fresh->GetOutput(), inverter->GetOutput()));
}

// Initial estimate, float, no boundary condition: estimate left unchanged, inverse correct, release flag agrees.
TEST(InvertDisplacementFieldImageFilter, InitialEstimateInFloat)
{
  const auto makeEstimate = [](const FloatFieldType * field) {
    auto estimate = FloatFieldType::New();
    estimate->CopyInformation(field);
    estimate->SetRegions(field->GetLargestPossibleRegion());
    estimate->Allocate();
    itk::ImageRegionConstIterator<FloatFieldType> itF(field, field->GetLargestPossibleRegion());
    itk::ImageRegionIterator<FloatFieldType>      itE(estimate, estimate->GetLargestPossibleRegion());
    for (; !itF.IsAtEnd(); ++itF, ++itE)
    {
      itE.Set(itF.Get() * -0.5f);
    }
    return estimate;
  };
  const auto run = [](const FloatFieldType * field, FloatFieldType * estimate) {
    auto inverter = MakeInverter(field);
    inverter->SetInverseFieldInitialEstimate(estimate);
    inverter->SetEnforceBoundaryCondition(false);
    inverter->Update();
    return inverter;
  };

  const auto field = MakeField<FloatFieldType>();
  const auto estimate = makeEstimate(field.GetPointer());
  const auto inverter = run(field.GetPointer(), estimate.GetPointer());
  ASSERT_NO_FATAL_FAILURE(ExpectSameField(makeEstimate(field.GetPointer()).GetPointer(), estimate.GetPointer()));
  const double residual = MaximumInteriorCompositionError(field.GetPointer(), inverter->GetOutput());
  EXPECT_LT(residual, 0.05 * MaximumInteriorNorm(field.GetPointer())) << "residual " << residual;

  const auto                       releasedField = MakeField<FloatFieldType>();
  const auto                       releasedEstimate = makeEstimate(releasedField.GetPointer());
  const GlobalReleaseDataFlagGuard releaseData;
  const auto                       released = run(releasedField.GetPointer(), releasedEstimate.GetPointer());
  ASSERT_NO_FATAL_FAILURE(ExpectSameField(inverter->GetOutput(), released->GetOutput()));
}
