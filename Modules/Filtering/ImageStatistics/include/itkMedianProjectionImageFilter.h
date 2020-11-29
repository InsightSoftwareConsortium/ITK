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
#ifndef itkMedianProjectionImageFilter_h
#define itkMedianProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

#include <vector>
#include <algorithm>

namespace itk
{
/** \class MedianProjectionImageFilter
 * \brief Median projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 * https://www.insight-journal.org/browse/publication/71
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel>
class MedianAccumulator
{
public:
  MedianAccumulator(SizeValueType size) { m_Values.reserve(size); }

  ~MedianAccumulator() = default;

  inline void
  Initialize()
  {
    m_Values.clear();
  }

  inline void
  operator()(const TInputPixel & input)
  {
    m_Values.push_back(input);
  }

  inline TInputPixel
  GetValue()
  {
    auto medianIterator = m_Values.begin() + m_Values.size() / 2;
    std::nth_element(m_Values.begin(), medianIterator, m_Values.end());
    return *medianIterator;
  }

  std::vector<TInputPixel> m_Values;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class MedianProjectionImageFilter
  : public ProjectionImageFilter<TInputImage, TOutputImage, Functor::MedianAccumulator<typename TInputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MedianProjectionImageFilter);

  using Self = MedianProjectionImageFilter;
  using Superclass =
    ProjectionImageFilter<TInputImage, TOutputImage, Functor::MedianAccumulator<typename TInputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(MedianProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // End concept checking
#endif

protected:
  MedianProjectionImageFilter() = default;
  ~MedianProjectionImageFilter() override = default;
}; // end MedianProjectionImageFilter
} // end namespace itk

#endif
