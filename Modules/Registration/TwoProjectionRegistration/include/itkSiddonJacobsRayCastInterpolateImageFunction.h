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
  ITK_DISALLOW_COPY_AND_MOVE(SiddonJacobsRayCastInterpolateImageFunction);

  /** Standard class type alias. */
  using Self = SiddonJacobsRayCastInterpolateImageFunction;
  using Superclass = InterpolateImageFunction<TInputImage, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Constants for the image dimensions */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;


  using TransformType = Euler3DTransform<TCoordRep>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  using PixelType = typename Superclass::InputPixelType;

  using SizeType = typename TInputImage::SizeType;

  using DirectionType = Vector<TCoordRep, 3>;

  /**  Type of the Interpolator Base class */
  using InterpolatorType = InterpolateImageFunction<TInputImage, TCoordRep>;

  using InterpolatorPointer = typename InterpolatorType::Pointer;


  /** Run-time type information (and related methods). */
  itkTypeMacro(SiddonJacobsRayCastInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType type alias support. */
  using OutputType = typename Superclass::OutputType;

  /** InputImageType type alias support. */
  using InputImageType = typename Superclass::InputImageType;

  /** InputImageConstPointer type alias support. */
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;

  /** RealType type alias support. */
  using RealType = typename Superclass::RealType;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Point type alias support. */
  using PointType = typename Superclass::PointType;

  /** Index type alias support. */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support. */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;


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
  OutputType
  Evaluate(const PointType & point) const override;

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
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  virtual void
  Initialize();

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
  IsInsideBuffer(const PointType &) const override
  {
    return true;
  }
  bool
  IsInsideBuffer(const ContinuousIndexType &) const override
  {
    return true;
  }
  bool
  IsInsideBuffer(const IndexType &) const override
  {
    return true;
  }

#if !defined(ITKV4_COMPATIBILITY)
  SizeType
  GetRadius() const override
  {
    const InputImageType * input = this->GetInputImage();
    if (!input)
    {
      itkExceptionMacro("Input image required!");
    }
    return input->GetLargestPossibleRegion().GetSize();
  }
#endif

protected:
  SiddonJacobsRayCastInterpolateImageFunction();

  ~SiddonJacobsRayCastInterpolateImageFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /// Transformation used to calculate the new focal point position
  TransformPointer m_Transform; // Displacement of the volume
  // Overall inverse transform used to calculate the ray position in the input space
  TransformPointer m_InverseTransform;

  // The threshold above which voxels along the ray path are integrated
  double m_Threshold;
  double m_FocalPointToIsocenterDistance; // Focal point to isocenter distance
  double m_ProjectionAngle;               // Linac gantry rotation angle in radians

private:
  void
                   ComputeInverseTransform() const;
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
