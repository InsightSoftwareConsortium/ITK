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
#ifndef itkStatisticsImageFilter_hxx
#define itkStatisticsImageFilter_hxx
#include "itkStatisticsImageFilter.h"


#include "itkImageScanlineIterator.h"
#include <mutex>

namespace itk
{
template <typename TInputImage>
StatisticsImageFilter<TInputImage>::StatisticsImageFilter()
{
  this->SetNumberOfRequiredInputs(1);

  Self::SetMinimum(NumericTraits<PixelType>::max());
  Self::SetMaximum(NumericTraits<PixelType>::NonpositiveMin());
  Self::SetMean(NumericTraits<RealType>::max());
  Self::SetSigma(NumericTraits<RealType>::max());
  Self::SetVariance(NumericTraits<RealType>::max());
  Self::SetSum(NumericTraits<RealType>::ZeroValue());
  Self::SetSumOfSquares(NumericTraits<RealType>::ZeroValue());
}

template <typename TInputImage>
DataObject::Pointer
StatisticsImageFilter<TInputImage>::MakeOutput(const DataObjectIdentifierType & name)
{
  if (name == "Minimum" || name == "Maximum")
  {
    return PixelObjectType::New();
  }
  if (name == "Mean" || name == "Sigma" || name == "Variance" || name == "Sum" || name == "SumOfSquares")
  {
    return RealObjectType::New();
  }
  return Superclass::MakeOutput(name);
}

template <typename TInputImage>
void
StatisticsImageFilter<TInputImage>::BeforeStreamedGenerateData()
{
  Superclass::BeforeStreamedGenerateData();

  // Resize the thread temporaries
  m_Count = NumericTraits<SizeValueType>::ZeroValue();
  m_SumOfSquares = NumericTraits<RealType>::ZeroValue();
  m_ThreadSum = NumericTraits<RealType>::ZeroValue();
  m_ThreadMin = NumericTraits<PixelType>::max();
  m_ThreadMax = NumericTraits<PixelType>::NonpositiveMin();
}

template <typename TInputImage>
void
StatisticsImageFilter<TInputImage>::AfterStreamedGenerateData()
{
  Superclass::AfterStreamedGenerateData();

  const SizeValueType count = m_Count;
  const RealType      sumOfSquares(m_SumOfSquares);
  const PixelType     minimum = m_ThreadMin;
  const PixelType     maximum = m_ThreadMax;
  const RealType      sum(m_ThreadSum);

  const RealType mean = sum / static_cast<RealType>(count);
  const RealType variance =
    (sumOfSquares - (sum * sum / static_cast<RealType>(count))) / (static_cast<RealType>(count) - 1);
  const RealType sigma = std::sqrt(variance);

  // Set the outputs
  this->SetMinimum(minimum);
  this->SetMaximum(maximum);
  this->SetMean(mean);
  this->SetSigma(sigma);
  this->SetVariance(variance);
  this->SetSum(sum);
  this->SetSumOfSquares(sumOfSquares);
}

template <typename TInputImage>
void
StatisticsImageFilter<TInputImage>::ThreadedStreamedGenerateData(const RegionType & regionForThread)
{

  CompensatedSummation<RealType> sum = NumericTraits<RealType>::ZeroValue();
  CompensatedSummation<RealType> sumOfSquares = NumericTraits<RealType>::ZeroValue();
  SizeValueType                  count = NumericTraits<SizeValueType>::ZeroValue();
  PixelType                      min = NumericTraits<PixelType>::max();
  PixelType                      max = NumericTraits<PixelType>::NonpositiveMin();

  ImageScanlineConstIterator<TInputImage> it(this->GetInput(), regionForThread);

  // do the work
  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      const PixelType & value = it.Get();
      const auto        realValue = static_cast<RealType>(value);
      min = std::min(min, value);
      max = std::max(max, value);

      sum += realValue;
      sumOfSquares += (realValue * realValue);
      ++count;
      ++it;
    }
    it.NextLine();
  }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_ThreadSum += sum;
  m_SumOfSquares += sumOfSquares;
  m_Count += count;
  m_ThreadMin = std::min(min, m_ThreadMin);
  m_ThreadMax = std::max(max, m_ThreadMax);
}

template <typename TImage>
void
StatisticsImageFilter<TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Count: " << static_cast<typename NumericTraits<SizeValueType>::PrintType>(this->m_Count)
     << std::endl;
  os << indent << "Minimum: " << static_cast<typename NumericTraits<PixelType>::PrintType>(this->GetMinimum())
     << std::endl;
  os << indent << "Maximum: " << static_cast<typename NumericTraits<PixelType>::PrintType>(this->GetMaximum())
     << std::endl;
  os << indent << "Sum: " << this->GetSum() << std::endl;
  os << indent << "Mean: " << this->GetMean() << std::endl;
  os << indent << "Sigma: " << this->GetSigma() << std::endl;
  os << indent << "Variance: " << this->GetVariance() << std::endl;
  os << indent << "SumOfSquares: " << this->GetSumOfSquares() << std::endl;
}
} // end namespace itk
#endif
