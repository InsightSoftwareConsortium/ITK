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
#ifndef itkInterpolateImageFunction_h
#define itkInterpolateImageFunction_h

#include "itkImageFunction.h"

namespace itk
{
/**
 *\class InterpolateImageFunction
 * \brief Base class for all image interpolators.
 *
 * InterpolateImageFunction is the base for all ImageFunctions that
 * interpolates image intensity at a non-integer pixel position.
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double ).
 *
 * \warning This hierarchy of functions work only for images
 * with scalar pixel types. For images of vector pixel types
 * use VectorInterpolateImageFunctions.
 *
 * \sa VectorInterpolateImageFunction
 * \ingroup ImageFunctions ImageInterpolators
 *
 *
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TCoordRep = double>
class ITK_TEMPLATE_EXPORT InterpolateImageFunction
  : public ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(InterpolateImageFunction);

  /** Standard class type aliases. */
  using Self = InterpolateImageFunction;
  using Superclass =
    ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImageFunction, ImageFunction);

  /** OutputType type alias support */
  using OutputType = typename Superclass::OutputType;

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** Size type alias support */
  using SizeType = typename InputImageType::SizeType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** RealType type alias support */
  using RealType = typename NumericTraits<typename TInputImage::PixelType>::RealType;

  /** Interpolate the image at a point position
   *
   * Returns the interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  Evaluate(const PointType & point) const override
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point, index);
    return (this->EvaluateAtContinuousIndex(index));
  }

  /** Interpolate the image at a continuous index position
   *
   * Returns the interpolated image intensity at a
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Subclasses must override this method.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override = 0;

  /** Interpolate the image at an index position.
   *
   * Simply returns the image value at the
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override
  {
    return (static_cast<RealType>(this->GetInputImage()->GetPixel(index)));
  }

/** Get the radius required for interpolation.
 *
 * This defines the number of surrounding pixels required to interpolate at
 * a given point.
 */
#if defined(ITKV4_COMPATIBILITY)
  virtual SizeType
  GetRadius() const
  {
    // if ITKv4 compatibility is enabled then set the radius to the
    // largest by default.
    const InputImageType * input = this->GetInputImage();
    if (!input)
    {
      itkExceptionMacro("Input image required!");
    }
    return input->GetLargestPossibleRegion().GetSize();
  }
#else
  virtual SizeType
  GetRadius() const = 0;
#endif

protected:
  InterpolateImageFunction() = default;
  ~InterpolateImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#endif
