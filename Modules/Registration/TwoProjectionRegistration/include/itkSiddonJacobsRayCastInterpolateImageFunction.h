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
/*=========================================================================
Calculate DRR from a CT dataset using incremental ray-tracing algorithm
The algorithm was initially proposed by Robert Siddon and improved by
Filip Jacobs etc.

-------------------------------------------------------------------------
References:

R. L. Siddon, "Fast calculation of the exact radiological path for a
threedimensional CT array," Medical Physics 12, 252-55 (1985).

F. Jacobs, E. Sundermann, B. De Sutter, M. Christiaens, and I. Lemahieu,
"A fast algorithm to calculate the exact radiological path through a pixel
or voxel space," Journal of Computing and Information Technology ?
CIT 6, 89-94 (1998).

=========================================================================*/
#ifndef itkSiddonJacobsRayCastInterpolateImageFunction_h
#define itkSiddonJacobsRayCastInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"
#include "itkTransform.h"
#include "itkVector.h"
#include "itkEuler3DTransform.h"

namespace itk
{

/** \class SiddonJacobsRayCastInterpolateImageFunction
 * \brief Projective interpolation of an image at specified positions.
 *
 * SiddonJacobsRayCastInterpolateImageFunction casts rays through a 3-dimensional
 * image
 * \warning This interpolator works for 3-dimensional images only.
 *
 * \ingroup ImageFunctions
 * \ingroup TwoProjectionRegistration
 */
template <typename TInputImage, typename TCoordRep = float>
class SiddonJacobsRayCastInterpolateImageFunction : public InterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  /** Standard class typedefs. */
  typedef SiddonJacobsRayCastInterpolateImageFunction      Self;
  typedef InterpolateImageFunction<TInputImage, TCoordRep> Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;

  /** Constants for the image dimensions */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);


  typedef Euler3DTransform<TCoordRep> TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  typedef typename Superclass::InputPixelType PixelType;

  typedef typename TInputImage::SizeType SizeType;

  typedef Vector<TCoordRep, 3> DirectionType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction<TInputImage, TCoordRep> InterpolatorType;

  typedef typename InterpolatorType::Pointer InterpolatorPointer;


  /** Run-time type information (and related methods). */
  itkTypeMacro(SiddonJacobsRayCastInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** InputImageConstPointer typedef support. */
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

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
  virtual OutputType
  Evaluate(const PointType & point) const;

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
  virtual OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const;

  virtual void
  Initialize(void);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);
  /** Get a pointer to the Transform.  */
  itkGetConstObjectMacro(Transform, TransformType);

  /** Set and get the focal point to isocenter distance in mm */
  itkSetMacro(FocalPointToIsocenterDistance, double);
  itkGetMacro(FocalPointToIsocenterDistance, double);

  /** Set and get the Lianc grantry rotation angle in radians */
  itkSetMacro(ProjectionAngle, double);
  itkGetMacro(ProjectionAngle, double);

  /** Set and get the Threshold */
  itkSetMacro(Threshold, double);
  itkGetMacro(Threshold, double);

  /** Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done. */
  inline bool
  IsInsideBuffer(const PointType &) const
  {
    return true;
  }
  bool
  IsInsideBuffer(const ContinuousIndexType &) const
  {
    return true;
  }
  bool
  IsInsideBuffer(const IndexType &) const
  {
    return true;
  }

protected:
  SiddonJacobsRayCastInterpolateImageFunction();

  ~SiddonJacobsRayCastInterpolateImageFunction() {};

  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /// Transformation used to calculate the new focal point position
  TransformPointer m_Transform; // Displacement of the volume
  // Overall inverse transform used to calculate the ray position in the input space
  TransformPointer m_InverseTransform;

  // The threshold above which voxels along the ray path are integrated
  double m_Threshold;
  double m_FocalPointToIsocenterDistance; // Focal point to isocenter distance
  double m_ProjectionAngle;               // Linac gantry rotation angle in radians

private:
  SiddonJacobsRayCastInterpolateImageFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
  void
                   ComputeInverseTransform(void) const;
  TransformPointer m_GantryRotTransform; // Gantry rotation transform
  TransformPointer m_CamShiftTransform;  // Camera shift transform camRotTransform
  TransformPointer m_CamRotTransform;    // Camera rotation transform
  TransformPointer m_ComposedTransform;  // Composed transform
  PointType        m_SourcePoint;        // Coordinate of the source in the standard Z projection geometry
  PointType        m_SourceWorld;        // Coordinate of the source in the world coordinate system
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSiddonJacobsRayCastInterpolateImageFunction.hxx"
#endif

#endif
