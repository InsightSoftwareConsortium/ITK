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
#ifndef itkTextureHistogram_h
#define itkTextureHistogram_h
#include "itkNumericTraits.h"

namespace itk
{
namespace Function
{

/* \class TextureHistogram
 *
 *
 *
 * \ingroup ITKTextureAnalysis
 */
template <class TInputPixel, class TOutputPixel>
class ITK_TEMPLATE_EXPORT TextureHistogram
{
public:
  TextureHistogram() { m_Count = 0; }

  // ~TextureHistogram()  {} default is ok

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
    typename MapType::iterator it = m_Map.find(p);

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
    // double median = 0.0;

    double entropy = 0.0;
    size_t curCount = 0;
    // typename MapType::iterator medianIt = m_Map.end();
    for (typename MapType::iterator i = m_Map.begin(); i != m_Map.end(); ++i)
    {
      double t = double(i->first) * double(i->second);
      sum += t;
      sum2 += (t *= double(i->first));
      sum3 += (t *= double(i->first));
      sum4 += (t *= double(i->first));

      curCount += i->second;

      const double p_x = double(i->second) / count;
      entropy += -p_x * vcl_log(p_x);

      //       // this is wrong!
      //       if ( curCount == count / 2 )
      //         {
      //         median += i->first;
      //         medianIt = il
      //         // we have an even number so take the average
      //           if ( !(count % 2) )
      //             {
      //               median *= 0.5;
      //             }
      //         }
    }

    //     curCount = 0;
    //     typename MapType::iterator fmedianIt = medianIt;
    //     typename MapType::iterator rmedianIt = medianIt;
    //     double mad = 0.0;

    //     while (curCount < count/2 )
    //     {
    //     if ( vcl_fabs( fmedianIt->first - median ) < vcl_fabs( rmedianIt->first - median ) )
    //       {
    //       curCount += fmedianIt->second;
    //       ++fmedianIt;
    //       }
    //     else
    //       {
    //       curCount += rmedianIt->second;
    //       --rmedianIt;
    //       }
    //     }


    const double icount = 1.0 / count;
    const double mean = sum * icount;

    // unbiased estimate
    const double variance = (sum2 - (sum * sum * icount)) / (count - 1);
    const double sigma = vcl_sqrt(variance);
    double       skewness = 0.0;
    double       kurtosis = 0.0;
    if (vcl_abs(variance * sigma) > itk::NumericTraits<double>::min())
    {

      skewness = ((sum3 - 3.0 * mean * sum2) * icount + 2.0 * mean * mean * mean) / (variance * sigma);
    }
    if (vcl_abs(variance) > itk::NumericTraits<double>::min())
    {
      kurtosis = (sum4 * icount + mean * (-4.0 * sum3 * icount + mean * (6.0 * sum2 * icount - 3.0 * mean * mean))) /
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
  typedef typename std::map<TInputPixel, size_t> MapType;

  MapType m_Map;
  size_t  m_Count;
};

} // end namespace Function
} // end namespace itk
#endif
