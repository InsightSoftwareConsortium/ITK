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
#ifndef __itkCentralDifferenceImageFunction_h
#define __itkCentralDifferenceImageFunction_h

#include "itkImageFunction.h"
#include "itkCovariantVector.h"
#include "itkInterpolateImageFunction.h"

namespace itk
{
/**
 * \class CentralDifferenceImageFunction
 * \brief Calculate the derivative by central differencing.
 *
 * This class is templated over the input image type and
 * the coordinate representation type (e.g. float or double).
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template<
  class TInputImage,
  class TCoordRep = float >
class ITK_EXPORT CentralDifferenceImageFunction:
  public ImageFunction< TInputImage,
                        CovariantVector< double, \
                                         TInputImage::ImageDimension >,
                        TCoordRep >
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef CentralDifferenceImageFunction Self;
  typedef ImageFunction< TInputImage,
                         CovariantVector< double,
                                          itkGetStaticConstMacro(ImageDimension) >,
                         TCoordRep >       Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Spacing typedef support. */
  typedef typename TInputImage::SpacingType SpacingType;

  /** Interpolator typedef support. */
  typedef InterpolateImageFunction< TInputImage, TCoordRep > InterpolatorType;
  typedef typename InterpolatorType::Pointer                 InterpolatorPointer;

  /** Set the input image.  This must be set by the user. */
  virtual void SetInputImage(const TInputImage *inputData);

  /** Set interpolator. The interpolator is used in the methods
   * \c Evaluate and \c EvaluateAtContinuousIndex. */
  virtual void SetInterpolator(InterpolatorType *interpolator);

  /** Get the interpolator. */
  itkGetConstObjectMacro( Interpolator, InterpolatorType );

  /** Evalulate the image derivative by central differencing at specified index.
   *
   *  No bounds checking is done.
   *  The point is assume to lie within the image buffer. If not, 0 is
   *  returned for the derivative without any error return.
   *
   *  If \c index lies on a boundary in a given dimension, 0 is returned for
   *  that dimension.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtIndex(const IndexType & index) const;

  /** Evalulate the image derivative by central differencing at non-integer
   *  point.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer. If not, 0 is
   *  returned for the derivative without any error return.
   *
   *  If \c point lies on a boundary in a given dimension, 0 is returned for
   *  that dimension.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType Evaluate(const PointType & point) const;

  /** Evalulate the image derivative by central differencing at non-integer
   *  index.
   *
   *  No bounds checking is done.
   *  The point is assume to lie within the image buffer. If not, 0 is
   *  returned for the derivative without any error return.
   *
   *  If \c cindex lies on a boundary in a given dimension, 0 is returned for
   *  that dimension.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex) const;

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not.
   * For \c EvaluateAtIndex and \c EvaluateAtContinuousIndex, the flag ON will
   * take into account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * For \c Evaluate, the opposite is true: the flag OFF will ignore the image
   * direction and will result in an extra matrix multiplication compared to the
   * amount of computation performed when the flag is ON.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);
protected:
  CentralDifferenceImageFunction();
  ~CentralDifferenceImageFunction(){}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  CentralDifferenceImageFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;

  // interpolator
  InterpolatorPointer   m_Interpolator;
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_CentralDifferenceImageFunction(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                                  \
  {                                                                              \
  _( 2 ( class EXPORT CentralDifferenceImageFunction< ITK_TEMPLATE_2 TypeX > ) ) \
  namespace Templates                                                            \
  {                                                                              \
  typedef CentralDifferenceImageFunction< ITK_TEMPLATE_2 TypeX >                 \
  CentralDifferenceImageFunction##TypeY;                                       \
  }                                                                              \
  }

#if ITK_TEMPLATE_EXPLICIT
// HACK:  template < class TInputImage, class TCoordRep >
//    const unsigned int
// CentralDifferenceImageFunction<TInputImage,TCoordRep>::ImageDimension;
#include "Templates/itkCentralDifferenceImageFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkCentralDifferenceImageFunction.hxx"
#endif

#endif
