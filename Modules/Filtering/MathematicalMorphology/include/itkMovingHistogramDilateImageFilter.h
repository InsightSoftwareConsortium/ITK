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
#ifndef itkMovingHistogramDilateImageFilter_h
#define itkMovingHistogramDilateImageFilter_h

#include "itkMovingHistogramMorphologyImageFilter.h"
#include <functional>

namespace itk
{
/**
 * \class MovingHistogramDilateImageFilter
 * \brief gray scale dilation of an image
 *
 * Dilate an image using grayscale morphology. Dilation takes the
 * maximum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionMorphologicalGradientImageFilter, BinaryMorphologicalGradientImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TOutputImage, typename TKernel>
class MovingHistogramDilateImageFilter
  : public MovingHistogramMorphologyImageFilter<
      TInputImage,
      TOutputImage,
      TKernel,
      typename Function::MorphologyHistogram<typename TInputImage::PixelType,
                                             typename std::greater<typename TInputImage ::PixelType>>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MovingHistogramDilateImageFilter);

  /** Standard class type aliases. */
  using Self = MovingHistogramDilateImageFilter;
  using Superclass = MovingHistogramMorphologyImageFilter<
    TInputImage,
    TOutputImage,
    TKernel,
    typename Function::MorphologyHistogram<typename TInputImage::PixelType,
                                           typename std::greater<typename TInputImage ::PixelType>>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramDilateImageFilter, MovingHistogramMorphologyImageFilter);

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

protected:
  MovingHistogramDilateImageFilter() { this->m_Boundary = NumericTraits<PixelType>::NonpositiveMin(); }

  ~MovingHistogramDilateImageFilter() override = default;
}; // end of class
} // end namespace itk

#endif
