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
#ifndef itkRayCastInterpolateImageFunction_h
#define itkRayCastInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"
#include "itkTransform.h"

namespace itk
{
/** \class RayCastInterpolateImageFunction
 * \brief Projective interpolation of an image at specified positions.
 *
 * RayCastInterpolateImageFunction casts rays through a 3-dimensional
 * image and uses bilinear interpolation to integrate each plane of
 * voxels traversed.
 *
 * \warning This interpolator works for 3-dimensional images only.
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = double >
class ITK_TEMPLATE_EXPORT RayCastInterpolateImageFunction:
  public InterpolateImageFunction< TInputImage, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef RayCastInterpolateImageFunction                    Self;
  typedef InterpolateImageFunction< TInputImage, TCoordRep > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

  /** Constants for the image dimensions */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /**
   * Type of the Transform Base class
   * The fixed image should be a 3D image
   */
  typedef Transform< TCoordRep, 3, 3 > TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  typedef typename Superclass::InputPixelType PixelType;

  typedef typename TInputImage::SizeType SizeType;

  typedef Vector< TCoordRep, 3 > DirectionType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction< TInputImage, TCoordRep > InterpolatorType;

  typedef typename InterpolatorType::Pointer InterpolatorPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RayCastInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** \brief
   * Interpolate the image at a point position.
   *
   * Returns the interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method.
   */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE;

  /** Interpolate the image at a continuous index position
   *
   * Returns the interpolated image intensity at a
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Subclasses must override this method.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method.
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE;

  /** Connect the Transform.
    * This Transformation is used to calculate the new focal point position.
    * */
  itkSetObjectMacro(Transform, TransformType);
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Connect the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  /** Get a pointer to the Interpolator.  */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** The focal point or position of the ray source. */
  itkSetMacro(FocalPoint, InputPointType);
  itkGetConstMacro(FocalPoint, InputPointType);

  /** The threshold above which voxels along the ray path are integrated. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  /** Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done. */
  bool IsInsideBuffer(const PointType &) const ITK_OVERRIDE
  {
    return true;
  }

  bool IsInsideBuffer(const ContinuousIndexType &) const ITK_OVERRIDE
  {
    return true;
  }

  bool IsInsideBuffer(const IndexType &) const ITK_OVERRIDE
  {
    return true;
  }

protected:
  RayCastInterpolateImageFunction();
  virtual ~RayCastInterpolateImageFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  TransformPointer    m_Transform;
  InputPointType      m_FocalPoint;
  double              m_Threshold;
  InterpolatorPointer m_Interpolator;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RayCastInterpolateImageFunction);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRayCastInterpolateImageFunction.hxx"
#endif

#endif
