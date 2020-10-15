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
#ifndef itkMovingHistogramImageFilterBase_h
#define itkMovingHistogramImageFilterBase_h

#include "itkKernelImageFilter.h"
#include "itkLexicographicCompare.h"
#include <list>
#include <map>
#include <set>

namespace itk
{
/**
 * \class MovingHistogramImageFilterBase
 * \brief Implements a generic moving histogram algorithm
 *
 * This filter is a base class to implement efficiently many neighborhood
 * filters. Instead of visiting all the neighbors of a pixel, the set
 * of pixels in the neighborhood is updated when the filter is moving
 * to a new pixel. The number of pixels read for each pixel can be very
 * smaller than the number of pixels read by a basic algorithm.
 *
 * This filter moves the neighborhood over all the pixels of the output requested region,
 * and pass the pixel added and removed of the neighborhood to the an
 * histogram class. This filter doesn't implement the histogram class - it
 * must be implement and passed as template parameter. The histogram class
 * is not necessary a real histogram. It can be implemented in many ways,
 * and only has to provide the methods described below.
 *
 * This filter takes 4 template parameters: the input and output image type,
 * the structuring element (or kernel) type, and the histogram type.
 * The input and output image must have the same number of dimension.
 *
 * The histogram type is a class which has to implements seven methods:
 * + a default constructor which takes no parameter.
 * + void AddPixel( const InputPixelType &p ) is called when a new pixel
 * is added to the histogram.
 * + void RemovePixel( const InputPixelType &p ) is called when a pixel
 * is removed of the histogram.
 * + void AddBoundary() is called when a pixel outside the image is added.
 * No value is provided: it's the responsibility to the histogram class to
 * get it if needed. This method can be kept empty to ignore the boundary
 * pixels.
 * + void RemoveBoundary() is called to when a pixel outside the image is removed.
 * No value is provided: it's the responsibility to the histogram class to
 * get it if needed. This method can be kept empty to ignore the boundary
 * pixels.
 * + AType GetValue() is called to set the value of the output image. AType
 * must be the output pixel type, or a type castable to the output pixel type.
 *
 * MovingHistogramImageFilterBase add the new pixels before removing the old ones,
 * so, if AddBoundary() is implemented and/or the kernel is symmetric, it is safe
 * to consider that the histogram will never be empty.
 *
 * One histogram is created for each thread by the method NewHistogram().
 * The NewHistogram() method can be overriden to pass some parameters to the
 * histogram.
 *
 * The neighborhood is defined by a structuring element, and must a
 * itk::Neighborhood object or a subclass.
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MovingWindowMeanImageFilter, RankImageFilter, MaskedMovingHistogramImageFilter,
 * \sa MovingHistogramMorphologicalGradientImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 *
 * \author Gaetan Lehmann
 * \author Richard Beare
 * \ingroup ITKImageFilterBase
 */

template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT MovingHistogramImageFilterBase : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MovingHistogramImageFilterBase);

  /** Standard class type aliases. */
  using Self = MovingHistogramImageFilterBase;
  using Superclass = KernelImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramImageFilterBase, KernelImageFilter);

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

  /** Set kernel (structuring element). */
  void
  SetKernel(const KernelType & kernel) override;

  itkGetConstMacro(PixelsPerTranslation, SizeValueType);

protected:
  MovingHistogramImageFilterBase();
  ~MovingHistogramImageFilterBase() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GetDirAndOffset(const IndexType LineStart,
                  const IndexType PrevLineStart,
                  OffsetType &    LineOffset,
                  OffsetType &    Changes,
                  int &           LineDirection);

  // store the added and removed pixel offset in a list
  OffsetMapType m_AddedOffsets;
  OffsetMapType m_RemovedOffsets;

  // store the offset of the kernel to initialize the histogram
  OffsetListType m_KernelOffsets;

  FixedArray<int, Self::ImageDimension> m_Axes;

  SizeValueType m_PixelsPerTranslation;

private:
  class DirectionCost
  {
  public:
    DirectionCost(int dimension, int count)
    {
      m_Dimension = dimension;
      m_Count = count;
    }

    /**
     * return true if the object is a worth choice for the best axis
     * than the object in parameter
     */
    inline bool
    operator<(const DirectionCost & dc) const
    {
      if (m_Count > dc.m_Count)
      {
        return true;
      }
      else if (m_Count < dc.m_Count)
      {
        return false;
      }
      else // if (m_Count == dc.m_Count)
      {
        return m_Dimension > dc.m_Dimension;
      }
    }

    int m_Dimension;
    int m_Count;
  };
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMovingHistogramImageFilterBase.hxx"
#endif

#endif
