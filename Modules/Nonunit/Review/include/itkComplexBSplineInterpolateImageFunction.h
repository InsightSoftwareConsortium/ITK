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
#ifndef itkComplexBSplineInterpolateImageFunction_h
#define itkComplexBSplineInterpolateImageFunction_h

#include "itkBSplineInterpolateImageFunction.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"

namespace itk
{
/**
 * \class ComplexBSplineInterpolateImageFunction.
 * \brief Complex wrapper around BSplineInterpolateImageFunction.
 *
 * A complex wrapper class that splits complex input image in two real-type
 * subimages containing real and imaginary parts, that are interpolated using
 * the standard itkBSplineInterpolateImageFunction. The same requirements apply
 * for this class: Set spline order before setting the input image!
 * Derivative support is currently not implemented
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/585
 *
 * \ingroup ImageInterpolators
 * \ingroup ITKReview
 */
template <typename TImageType, typename TCoordRep = double, typename TCoefficientType = double>
class ITK_TEMPLATE_EXPORT ComplexBSplineInterpolateImageFunction
  : public InterpolateImageFunction<TImageType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ComplexBSplineInterpolateImageFunction);

  /** Standard class type alias. */
  using Self = ComplexBSplineInterpolateImageFunction;
  /** Standard class type alias. */
  using Superclass = InterpolateImageFunction<TImageType, TCoordRep>;
  /** Standard class type alias. */
  using Pointer = SmartPointer<Self>;
  /** Standard class type alias. */
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComplexBSplineInterpolateImageFunction, InterpolateImageFunction);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** OutputType type alias support */
  using typename Superclass::OutputType;

  /** InputImageType type alias support */
  using typename Superclass::InputImageType;

  /** Index type alias support */
  using typename Superclass::IndexType;

  /** Size type alias support */
  using typename Superclass::SizeType;

  /** ContinuousIndex type alias support */
  using typename Superclass::ContinuousIndexType;

  /** PointType type alias support */
  using typename Superclass::PointType;

  /** Internal Real and imaginary image type */
  using InternalImageType = Image<double, Self::ImageDimension>;

  /** Complex to Real filter type */
  using RealFilterType = ComplexToRealImageFilter<InputImageType, InternalImageType>;
  using ImaginaryFilterType = ComplexToImaginaryImageFilter<InputImageType, InternalImageType>;

  /** Underlying real BSpline interpolator */
  using InterpolatorType = BSplineInterpolateImageFunction<InternalImageType, TCoordRep, TCoefficientType>;

  /** Evaluate the function at a ContinuousIndex position.
   *
   * Returns the B-Spline interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assumed to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  /** Derivative type alias support */
  /*  using CovariantVectorType = CovariantVector< OutputType, Self::ImageDimension  >;

    CovariantVectorType EvaluateDerivative( const PointType & point ) const
    {
     ContinuousIndexType index;
     this->GetInputImage()->TransformPhysicalPointToContinuousIndex( point, index );
     return ( this->EvaluateDerivativeAtContinuousIndex( index ) );
    }

    CovariantVectorType EvaluateDerivativeAtContinuousIndex( const ContinuousIndexType & x ) const;
  */

  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void
  SetSplineOrder(unsigned int SplineOrder);

  itkGetConstMacro(SplineOrder, unsigned int);

  /** Set the input image.  This must be set by the user, after setting the
    spline order! */
  void
  SetInputImage(const TImageType * inputData) override;

  SizeType
  GetRadius() const override
  {
    return SizeType::Filled(m_SplineOrder + 1);
  }

protected:
  ComplexBSplineInterpolateImageFunction();
  ~ComplexBSplineInterpolateImageFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_SplineOrder;

  typename InterpolatorType::Pointer m_RealInterpolator;
  typename InterpolatorType::Pointer m_ImaginaryInterpolator;

  typename RealFilterType::Pointer m_RealFilter;

  typename ImaginaryFilterType::Pointer m_ImaginaryFilter;
}; // class
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkComplexBSplineInterpolateImageFunction.hxx"
#endif

#endif
