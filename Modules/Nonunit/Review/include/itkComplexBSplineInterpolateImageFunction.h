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
template< typename TImageType, typename TCoordRep = double, typename TCoefficientType = double >
class ITK_TEMPLATE_EXPORT ComplexBSplineInterpolateImageFunction:
  public InterpolateImageFunction< TImageType, TCoordRep >
{
public:
  /** Standard class typedef. */
  typedef ComplexBSplineInterpolateImageFunction Self;
  /** Standard class typedef. */
  typedef InterpolateImageFunction< TImageType, TCoordRep > Superclass;
  /** Standard class typedef. */
  typedef SmartPointer< Self > Pointer;
  /** Standard class typedef. */
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComplexBSplineInterpolateImageFunction, InterpolateImageFunction);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** PointType typedef support */
  typedef typename Superclass::PointType PointType;

  /** Internal Real and imaginary image type */
  typedef Image< double, itkGetStaticConstMacro(ImageDimension) > InternalImageType;

  /** Complex to Real filter type */
  typedef ComplexToRealImageFilter< InputImageType, InternalImageType >      RealFilterType;
  typedef ComplexToImaginaryImageFilter< InputImageType, InternalImageType > ImaginaryFilterType;

  /** Underlying real BSpline interpolator */
  typedef BSplineInterpolateImageFunction< InternalImageType, TCoordRep, TCoefficientType > InterpolatorType;

  /** Evaluate the function at a ContinuousIndex position.
  *
  * Returns the B-Spline interpolated image intensity at a
  * specified point position. No bounds checking is done.
  * The point is assumed to lie within the image buffer.
  *
  * ImageFunction::IsInsideBuffer() can be used to check bounds before
  * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex(const ContinuousIndexType & index) const ITK_OVERRIDE;

  /** Derivative typedef support */
/*  typedef CovariantVector< OutputType, itkGetStaticConstMacro( ImageDimension ) > CovariantVectorType;

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
  void SetSplineOrder(unsigned int SplineOrder);

  itkGetConstMacro(SplineOrder, int);

  /** Set the input image.  This must be set by the user, after setting the
    spline order! */
  virtual void SetInputImage(const TImageType *inputData) ITK_OVERRIDE;

protected:
  ComplexBSplineInterpolateImageFunction();
  virtual ~ComplexBSplineInterpolateImageFunction() {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComplexBSplineInterpolateImageFunction);

  unsigned int m_SplineOrder;

  typename InterpolatorType::Pointer m_RealInterpolator;
  typename InterpolatorType::Pointer m_ImaginaryInterpolator;

  typename RealFilterType::Pointer m_RealFilter;

  typename ImaginaryFilterType::Pointer m_ImaginaryFilter;
}; // class
} // namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkComplexBSplineInterpolateImageFunction.hxx"
#endif

#endif
