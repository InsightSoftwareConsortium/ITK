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
#ifndef itkCorrelationCoefficientHistogramImageToImageMetric_hxx
#define itkCorrelationCoefficientHistogramImageToImageMetric_hxx

#include "itkMath.h"

namespace itk
{
template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::EvaluateMeasure(
  HistogramType & histogram) const -> MeasureType
{
  const MeasureType varianceX = this->VarianceX(histogram);
  const MeasureType varianceY = this->VarianceY(histogram);
  const MeasureType covariance = this->Covariance(histogram);

  return itk::Math::abs(covariance / (std::sqrt(varianceX) * std::sqrt(varianceY)));
}

template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::MeanX(HistogramType & histogram) const
  -> MeasureType
{
  MeasureType meanX{};

  for (unsigned int i = 0; i < this->m_HistogramSize[0]; ++i)
  {
    const MeasureType            valX = histogram.GetMeasurement(i, 0);
    const HistogramFrequencyType freq = histogram.GetFrequency(i, 0);
    meanX += valX * freq;
  }

  meanX /= histogram.GetTotalFrequency();

  return meanX;
}

template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::MeanY(HistogramType & histogram) const
  -> MeasureType
{
  MeasureType meanY{};

  for (unsigned int i = 0; i < this->m_HistogramSize[1]; ++i)
  {
    const MeasureType            valY = histogram.GetMeasurement(i, 1);
    const HistogramFrequencyType freq = histogram.GetFrequency(i, 1);
    meanY += valY * freq;
  }

  meanY /= histogram.GetTotalFrequency();

  return meanY;
}

template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::VarianceX(HistogramType & histogram) const
  -> MeasureType
{
  MeasureType varX{};

  for (unsigned int i = 0; i < this->m_HistogramSize[0]; ++i)
  {
    varX += static_cast<double>(histogram.GetFrequency(i, 0)) / histogram.GetTotalFrequency() *
            Math::sqr(histogram.GetMeasurement(i, 0));
  }

  return varX - Math::sqr(MeanX(histogram));
}

template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::VarianceY(HistogramType & histogram) const
  -> MeasureType
{
  MeasureType varY{};

  for (unsigned int i = 0; i < this->m_HistogramSize[1]; ++i)
  {
    varY += static_cast<double>(histogram.GetFrequency(i, 1)) / histogram.GetTotalFrequency() *
            Math::sqr(histogram.GetMeasurement(i, 1));
  }

  return varY - Math::sqr(MeanY(histogram));
}

template <typename TFixedImage, typename TMovingImage>
auto
CorrelationCoefficientHistogramImageToImageMetric<TFixedImage, TMovingImage>::Covariance(
  HistogramType & histogram) const -> MeasureType
{
  MeasureType       var{};
  const MeasureType meanX = MeanX(histogram);
  const MeasureType meanY = MeanY(histogram);

  for (unsigned int j = 0; j < this->m_HistogramSize[1]; ++j)
  {
    for (unsigned int i = 0; i < this->m_HistogramSize[0]; ++i)
    {
      typename HistogramType::IndexType index;
      index.SetSize(2);
      index[0] = i;
      index[1] = j;

      var += histogram.GetFrequency(index) * (histogram.GetMeasurement(i, 0) - meanX) *
             (histogram.GetMeasurement(j, 1) - meanY);
    }
  }

  var /= histogram.GetTotalFrequency();

  return var;
}
} // namespace itk

#endif // itkCorrelationCoefficientHistogramImageToImageMetric_hxx
