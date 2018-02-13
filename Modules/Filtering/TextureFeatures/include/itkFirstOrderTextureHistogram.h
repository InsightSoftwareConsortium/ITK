/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkFirstOrderTextureHistogram_h
#define itkFirstOrderTextureHistogram_h

#include "itkNumericTraits.h"
#include "itkMath.h"
#include <map>

namespace itk
{
namespace Function
{

/* \class FirstOrderTextureHistogram
 *
 * An implementation of the "MovingHistogram" interface for the
 * MovingHistogramImageFilter class. This implementation maintains a
 * std::map based "histogram" during iteration and computes first
 * order statistics from the histogram.
 *
 * \ingroup ITKTextureFeatures
 */
template <class TInputPixel, class TOutputPixel>
class ITK_TEMPLATE_EXPORT FirstOrderTextureHistogram
{
public:
  FirstOrderTextureHistogram() { m_Count = 0; }

  void
  AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
    ++m_Count;
  }

  void
  RemovePixel(const TInputPixel & p)
  {

    // insert new item if one doesn't exist
    auto it = m_Map.find(p);

    assert(it != m_Map.end());

    if (--(it->second) == 0)
    {
      m_Map.erase(it);
    }
    --m_Count;
  }

  TOutputPixel
  GetValue(const TInputPixel &)
  {
    TOutputPixel out;
    NumericTraits<TOutputPixel>::SetLength(out, 8);

    double       sum = 0.0;
    double       sum2 = 0.0;
    double       sum3 = 0.0;
    double       sum4 = 0.0;
    const size_t count = m_Count;

    double entropy = 0.0;
    size_t curCount = 0;

    for (auto i = m_Map.begin(); i != m_Map.end(); ++i)
    {
      double t = double(i->first) * double(i->second);
      sum += t;
      sum2 += (t *= double(i->first));
      sum3 += (t *= double(i->first));
      sum4 += (t *= double(i->first));

      curCount += i->second;

      const double p_x = double(i->second) / count;
      entropy += -p_x * std::log(p_x) / itk::Math::ln2;
    }

    const double mean = sum / count;

    // unbiased estimate
    const double variance = (sum2 - (sum * sum) / count) / (count - 1);
    const double sigma = std::sqrt(variance);
    double       skewness = 0.0;
    double       kurtosis = 0.0;
    if (std::abs(variance * sigma) > itk::NumericTraits<double>::min())
    {

      skewness = ((sum3 - 3.0 * mean * sum2) / count + 2.0 * mean * mean * mean) / (variance * sigma);
    }
    if (std::abs(variance) > itk::NumericTraits<double>::min())
    {
      kurtosis = (sum4 / count + mean * (-4.0 * sum3 / count + mean * (6.0 * sum2 / count - 3.0 * mean * mean))) /
                   (variance * variance) -
                 3.0;
    }

    unsigned int i = 0;
    out[i++] = mean;
    out[i++] = m_Map.begin()->first;
    out[i++] = m_Map.rbegin()->first;
    out[i++] = variance;
    out[i++] = sigma;
    out[i++] = skewness;
    out[i++] = kurtosis;
    out[i++] = entropy;
    return out;
  }

  void
  AddBoundary()
  {}

  void
  RemoveBoundary()
  {}

private:
  using MapType = typename std::map<TInputPixel, size_t>;

  MapType m_Map;
  size_t  m_Count;
};

} // end namespace Function
} // end namespace itk
#endif
