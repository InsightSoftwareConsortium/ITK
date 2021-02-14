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
#ifndef itkPolylineMask2DImageFilter_h
#define itkPolylineMask2DImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/**
 *\class PolylineMask2DImageFilter
 * \brief Implements 2D image masking operation constrained by a contour.
 *
 * This class is parameterized over input image type, contour defined by a polyline,
 * and output image type. If the input image is three dimensional, the masking operation is
 * performed on each slice (2D image). The output image will have two regions demarcated
 * by the contour i.e inside(masked) and outside region.  The pixels in the
 * masked region will keep their original intensity values. Whereas, intensity
 * value of pixels outside the masked region will be set to zero.
 *
 * \warning The Polygon represented by the input Polyline must be a convex polygon.
 * \warning The Polygon's inside is defined by the standard clock-wise
 * convention. That is, when walking along the polygon, the inside of the
 * polygon is at the right-hand side. Note also that this must be interpreted
 * in the natural coordinate system used by ITK, not the one used in computer
 * graphics.
 *
 * \ingroup ImageToImageFilter
 * \sa  PolylineMaskImageFilter
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TPolyline, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PolylineMask2DImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PolylineMask2DImageFilter);

  /** Standard class type aliases. */
  using Self = PolylineMask2DImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolylineMask2DImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int NDimensions = TInputImage::ImageDimension;
  static constexpr unsigned int NOutputDimensions = TOutputImage::ImageDimension;

  /** Some convenient type alias for input image */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /* type alias for the polyline type */
  using PolylineType = TPolyline;

  /* type alias for the output image */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Read in image and polyline inputs */
  void
  SetInput1(const InputImageType * input);

  void
  SetInput2(const PolylineType * input);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<NDimensions, NOutputDimensions>));
  itkConceptMacro(IntConvertibleOutputCheck, (Concept::Convertible<int, OutputImagePixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  // End concept checking
#endif

protected:
  PolylineMask2DImageFilter();
  ~PolylineMask2DImageFilter() override = default;

  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolylineMask2DImageFilter.hxx"
#endif

#endif
