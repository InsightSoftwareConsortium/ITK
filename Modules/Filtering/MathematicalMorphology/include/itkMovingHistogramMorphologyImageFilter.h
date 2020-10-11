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
#ifndef itkMovingHistogramMorphologyImageFilter_h
#define itkMovingHistogramMorphologyImageFilter_h

#include "itkMorphologyHistogram.h"
#include "itkMovingHistogramImageFilter.h"
#include "itkLexicographicCompare.h"
#include <list>
#include <map>

namespace itk
{

/**
 * \class MovingHistogramMorphologyImageFilter
 * \brief Base class for MovingHistogramDilateImageFilter and MovingHistogramErodeImageFilter.
 *
 * This class is similar to MovingHistogramImageFilter, but adds support for
 * boundaries and does not fully update the histogram in order to enhance
 * performance.
 *
 * \sa MovingHistogramImageFilter, MovingHistogramDilateImageFilter, MovingHistogramErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram>
class ITK_TEMPLATE_EXPORT MovingHistogramMorphologyImageFilter
  : public MovingHistogramImageFilter<TInputImage, TOutputImage, TKernel, THistogram>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MovingHistogramMorphologyImageFilter);

  /** Standard class type aliases. */
  using Self = MovingHistogramMorphologyImageFilter;
  using Superclass = MovingHistogramImageFilter<TInputImage, TOutputImage, TKernel, THistogram>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramMorphologyImageFilter, MovingHistogramImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename KernelType::SizeType;

  using OffsetListType = typename std::list<OffsetType>;

  using OffsetMapType = typename std::map<OffsetType, OffsetListType, Functor::LexicographicCompare>;

  /** Set/Get the boundary value.
   *  Subclasses should set their own values. */
  itkSetMacro(Boundary, PixelType);
  itkGetConstMacro(Boundary, PixelType);

  /** Return true if the vector based algorithm is used, and
   * false if the map based algorithm is used */
  static bool
  GetUseVectorBasedAlgorithm()
  {
    return THistogram::UseVectorBasedAlgorithm();
  }

protected:
  MovingHistogramMorphologyImageFilter();
  ~MovingHistogramMorphologyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Multi-thread version GenerateData. */
  //   void  ThreadedGenerateData (const OutputImageRegionType&
  //                               outputRegionForThread,
  //                               ThreadIdType threadId);

  /** Configure the histogram.
   *  Used by this class to pass the boundary value to the histogram object. */
  void
  ConfigureHistogram(THistogram & histogram) override;

  PixelType m_Boundary;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMovingHistogramMorphologyImageFilter.hxx"
#endif

#endif
