/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkHistogramToRunLengthFeaturesFilter_hxx
#define itkHistogramToRunLengthFeaturesFilter_hxx

#include "itkHistogramToRunLengthFeaturesFilter.h"

#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{

// constructor
template <typename THistogram>
HistogramToRunLengthFeaturesFilter<THistogram>::HistogramToRunLengthFeaturesFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);

  // allocate the data objects for the outputs which are
  // just decorators real types
  for (unsigned int i = 0; i < 10; i++)
  {
    this->ProcessObject::SetNthOutput(i, this->MakeOutput(i));
  }
}

template <typename THistogram>
void
HistogramToRunLengthFeaturesFilter<THistogram>::SetInput(const HistogramType * histogram)
{
  this->ProcessObject::SetNthInput(0, const_cast<HistogramType *>(histogram));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::HistogramType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
  {
    return nullptr;
  }
  return itkDynamicCastInDebugMode<const HistogramType *>(this->ProcessObject::GetInput(0));
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::DataObjectPointer
HistogramToRunLengthFeaturesFilter<THistogram>::MakeOutput(DataObjectPointerArraySizeType itkNotUsed(idx))
{
  return MeasurementObjectType::New().GetPointer();
}

template <typename THistogram>
void
HistogramToRunLengthFeaturesFilter<THistogram>::GenerateData()
{
  const HistogramType * inputHistogram = this->GetInput();

  this->m_TotalNumberOfRuns = static_cast<unsigned long>(inputHistogram->GetTotalFrequency());

  MeasurementType shortRunEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType longRunEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType greyLevelNonuniformity = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType runLengthNonuniformity = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType lowGreyLevelRunEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType highGreyLevelRunEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType shortRunLowGreyLevelEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType shortRunHighGreyLevelEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType longRunLowGreyLevelEmphasis = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType longRunHighGreyLevelEmphasis = NumericTraits<MeasurementType>::ZeroValue();

  vnl_vector<double> greyLevelNonuniformityVector(inputHistogram->GetSize()[0], 0.0);
  vnl_vector<double> runLengthNonuniformityVector(inputHistogram->GetSize()[1], 0.0);

  using HistogramIterator = typename HistogramType::ConstIterator;
  for (HistogramIterator hit = inputHistogram->Begin(); hit != inputHistogram->End(); ++hit)
  {
    MeasurementType frequency = hit.GetFrequency();
    if (Math::ExactlyEquals(frequency, NumericTraits<MeasurementType>::ZeroValue()))
    {
      continue;
    }
    MeasurementVectorType measurement = hit.GetMeasurementVector();
    IndexType             index = inputHistogram->GetIndex(hit.GetInstanceIdentifier());

    auto i2 = static_cast<double>((index[0] + 1) * (index[0] + 1));
    auto j2 = static_cast<double>((index[1] + 1) * (index[1] + 1));

    // Traditional measures
    shortRunEmphasis += (frequency / j2);
    longRunEmphasis += (frequency * j2);

    greyLevelNonuniformityVector[index[0]] += frequency;
    runLengthNonuniformityVector[index[1]] += frequency;

    // measures from Chu et al.
    lowGreyLevelRunEmphasis += (frequency / i2);
    highGreyLevelRunEmphasis += (frequency * i2);

    // measures from Dasarathy and Holder
    shortRunLowGreyLevelEmphasis += (frequency / (i2 * j2));
    shortRunHighGreyLevelEmphasis += (frequency * i2 / j2);
    longRunLowGreyLevelEmphasis += (frequency * j2 / i2);
    longRunHighGreyLevelEmphasis += (frequency * i2 * j2);
  }
  greyLevelNonuniformity = greyLevelNonuniformityVector.squared_magnitude();
  runLengthNonuniformity = runLengthNonuniformityVector.squared_magnitude();

  // Normalize all measures by the total number of runs

  shortRunEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  longRunEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  greyLevelNonuniformity /= static_cast<double>(this->m_TotalNumberOfRuns);
  runLengthNonuniformity /= static_cast<double>(this->m_TotalNumberOfRuns);

  lowGreyLevelRunEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  highGreyLevelRunEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);

  shortRunLowGreyLevelEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  shortRunHighGreyLevelEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  longRunLowGreyLevelEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);
  longRunHighGreyLevelEmphasis /= static_cast<double>(this->m_TotalNumberOfRuns);

  auto * shortRunEmphasisOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(0));
  shortRunEmphasisOutputObject->Set(shortRunEmphasis);

  auto * longRunEmphasisOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(1));
  longRunEmphasisOutputObject->Set(longRunEmphasis);

  auto * greyLevelNonuniformityOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(2));
  greyLevelNonuniformityOutputObject->Set(greyLevelNonuniformity);

  auto * runLengthNonuniformityOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(3));
  runLengthNonuniformityOutputObject->Set(runLengthNonuniformity);

  auto * lowGreyLevelRunEmphasisOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(4));
  lowGreyLevelRunEmphasisOutputObject->Set(lowGreyLevelRunEmphasis);

  auto * highGreyLevelRunEmphasisOutputObject = static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(5));
  highGreyLevelRunEmphasisOutputObject->Set(highGreyLevelRunEmphasis);

  auto * shortRunLowGreyLevelEmphasisOutputObject =
    static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(6));
  shortRunLowGreyLevelEmphasisOutputObject->Set(shortRunLowGreyLevelEmphasis);

  auto * shortRunHighGreyLevelEmphasisOutputObject =
    static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(7));
  shortRunHighGreyLevelEmphasisOutputObject->Set(shortRunHighGreyLevelEmphasis);

  auto * longRunLowGreyLevelEmphasisOutputObject =
    static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(8));
  longRunLowGreyLevelEmphasisOutputObject->Set(longRunLowGreyLevelEmphasis);

  auto * longRunHighGreyLevelEmphasisOutputObject =
    static_cast<MeasurementObjectType *>(this->ProcessObject::GetOutput(9));
  longRunHighGreyLevelEmphasisOutputObject->Set(longRunHighGreyLevelEmphasis);
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(0));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(1));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetGreyLevelNonuniformityOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(2));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetRunLengthNonuniformityOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(3));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetLowGreyLevelRunEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(4));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetHighGreyLevelRunEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(5));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunLowGreyLevelEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(6));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunHighGreyLevelEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(7));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunLowGreyLevelEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(8));
}

template <typename THistogram>
const typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementObjectType *
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunHighGreyLevelEmphasisOutput() const
{
  return itkDynamicCastInDebugMode<const MeasurementObjectType *>(this->ProcessObject::GetOutput(9));
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunEmphasis() const
{
  return this->GetShortRunEmphasisOutput()->Get();
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunEmphasis() const
{
  return this->GetLongRunEmphasisOutput()->Get();
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetGreyLevelNonuniformity() const
{
  return this->GetGreyLevelNonuniformityOutput()->Get();
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetRunLengthNonuniformity() const
{
  return this->GetRunLengthNonuniformityOutput()->Get();
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetLowGreyLevelRunEmphasis() const
{
  return this->GetLowGreyLevelRunEmphasisOutput()->Get();
}
template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetHighGreyLevelRunEmphasis() const
{
  return this->GetHighGreyLevelRunEmphasisOutput()->Get();
}
template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunLowGreyLevelEmphasis() const
{
  return this->GetShortRunLowGreyLevelEmphasisOutput()->Get();
}
template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetShortRunHighGreyLevelEmphasis() const
{
  return this->GetShortRunHighGreyLevelEmphasisOutput()->Get();
}
template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunLowGreyLevelEmphasis() const
{
  return this->GetLongRunLowGreyLevelEmphasisOutput()->Get();
}
template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetLongRunHighGreyLevelEmphasis() const
{
  return this->GetLongRunHighGreyLevelEmphasisOutput()->Get();
}

template <typename THistogram>
typename HistogramToRunLengthFeaturesFilter<THistogram>::MeasurementType
HistogramToRunLengthFeaturesFilter<THistogram>::GetFeature(RunLengthFeatureEnum feature)
{
  switch (feature)
  {
    case RunLengthFeatureEnum::ShortRunEmphasis:
      return this->GetShortRunEmphasis();
    case RunLengthFeatureEnum::LongRunEmphasis:
      return this->GetLongRunEmphasis();
    case RunLengthFeatureEnum::GreyLevelNonuniformity:
      return this->GetGreyLevelNonuniformity();
    case RunLengthFeatureEnum::RunLengthNonuniformity:
      return this->GetRunLengthNonuniformity();
    case RunLengthFeatureEnum::LowGreyLevelRunEmphasis:
      return this->GetLowGreyLevelRunEmphasis();
    case RunLengthFeatureEnum::HighGreyLevelRunEmphasis:
      return this->GetHighGreyLevelRunEmphasis();
    case RunLengthFeatureEnum::ShortRunLowGreyLevelEmphasis:
      return this->GetShortRunLowGreyLevelEmphasis();
    case RunLengthFeatureEnum::ShortRunHighGreyLevelEmphasis:
      return this->GetShortRunHighGreyLevelEmphasis();
    case RunLengthFeatureEnum::LongRunLowGreyLevelEmphasis:
      return this->GetLongRunLowGreyLevelEmphasis();
    case RunLengthFeatureEnum::LongRunHighGreyLevelEmphasis:
      return this->GetLongRunHighGreyLevelEmphasis();
    default:
      return 0;
  }
}

template <typename THistogram>
void
HistogramToRunLengthFeaturesFilter<THistogram>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end of namespace Statistics
} // end of namespace itk


#endif
