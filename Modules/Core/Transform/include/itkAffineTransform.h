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
#ifndef itkAffineTransform_h
#define itkAffineTransform_h

#include "itkMatrixOffsetTransformBase.h"
#include <iostream>


namespace itk
{
/**
 * Affine transformation of a vector space (e.g. space coordinates)
 *
 * This class allows the definition and manipulation of affine
 * transformations of an n-dimensional affine space (and its
 * associated vector space) onto itself.  One common use is to define
 * and manipulate Euclidean coordinate transformations in two and
 * three dimensions, but other uses are possible as well.
 *
 * An affine transformation is defined mathematically as a linear
 * transformation plus a constant offset.  If A is a constant n x n
 * matrix and b is a constant n-vector, then y = Ax+b defines an
 * affine transformation from the n-vector x to the n-vector y.
 *
 * The difference between two points is a vector and transforms
 * linearly, using the matrix only.  That is, (y1-y2) = A*(x1-x2).
 *
 * The AffineTransform class determines whether to transform an object
 * as a point or a vector by examining its type.  An object of type
 * Point transforms as a point; an object of type Vector transforms as
 * a vector.
 *
 * One common use of affine transformations is to define coordinate
 * conversions in two- and three-dimensional space.  In this
 * application, x is a two- or three-dimensional vector containing the
 * "source" coordinates of a point, y is a vector containing the
 * "target" coordinates, the matrix A defines the scaling and rotation
 * of the coordinate systems from the source to the target, and b
 * defines the translation of the origin from the source to the
 * target.  More generally, A can also define anisotropic scaling and
 * shearing transformations.  Any good textbook on computer graphics
 * will discuss coordinate transformations in more detail.  Several of
 * the methods in this class are designed for this purpose and use the
 * language appropriate to coordinate conversions.
 *
 * Any two affine transformations may be composed and the result is
 * another affine transformation.  However, the order is important.
 * Given two affine transformations T1 and T2, we will say that
 * "precomposing T1 with T2" yields the transformation which applies
 * T1 to the source, and then applies T2 to that result to obtain the
 * target.  Conversely, we will say that "postcomposing T1 with T2"
 * yields the transformation which applies T2 to the source, and then
 * applies T1 to that result to obtain the target.  (Whether T1 or T2
 * comes first lexicographically depends on whether you choose to
 * write mappings from right-to-left or vice versa; we avoid the whole
 * problem by referring to the order of application rather than the
 * textual order.)
 *
 * There are two template parameters for this class:
 *
 * TParametersValueType The type to be used for scalar numeric
 *                      values.  Either float or double.
 *
 * NDimensions   The number of dimensions of the vector space.
 *
 * This class provides several methods for setting the matrix and vector
 * defining the transform. To support the registration framework, the
 * transform parameters can also be set as an Array<double> of size
 * (NDimension + 1) * NDimension using method SetParameters().
 * The first (NDimension x NDimension) parameters defines the matrix in
 * row-major order (where the column index varies the fastest).
 * The last NDimension parameters defines the translation
 * in each dimensions.
 *
 * This class also supports the specification of a center of rotation (center)
 * and a translation that is applied with respect to that centered rotation.
 * By default the center of rotation is set to the origin.
 *
 * \ingroup ITKTransform
 */

template <typename TParametersValueType = double, unsigned int NDimensions = 3>
// Number of dimensions in the input space
class ITK_TEMPLATE_EXPORT AffineTransform
  : public MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AffineTransform);

  /** Standard type alias   */
  using Self = AffineTransform;
  using Superclass = MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(AffineTransform, MatrixOffsetTransformBase);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NDimensions;
  static constexpr unsigned int OutputSpaceDimension = NDimensions;
  static constexpr unsigned int SpaceDimension = NDimensions;
  static constexpr unsigned int ParametersDimension = NDimensions * (NDimensions + 1);

  /** Parameters Type   */
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;
  using ScalarType = typename Superclass::ScalarType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;
  using MatrixType = typename Superclass::MatrixType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using CenterType = typename Superclass::CenterType;
  using OffsetType = typename Superclass::OffsetType;
  using TranslationType = typename Superclass::TranslationType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Compose affine transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise.
   * This updates Translation based on current center. */
  void
  Translate(const OutputVectorType & trans, bool pre = false);

  /** Compose affine transformation with a scaling
   *
   * This method modifies self to magnify the source by a given
   * factor along each axis.  If all factors are the same, or only a
   * single factor is given, then the scaling is isotropic;
   * otherwise it is anisotropic.  If an odd number of factors are
   * negative, then the parity of the image changes.  If any of the
   * factors is zero, then the transformation becomes a projection
   * and is not invertible.  The scaling is precomposed with self if
   * pre is true, and postcomposed otherwise.
   * Note that the scaling is applied centered at the origin. */
  void
  Scale(const OutputVectorType & factor, bool pre = false);

  void
  Scale(const TParametersValueType & factor, bool pre = false);

  /** Compose affine transformation with an elementary rotation
   *
   * This method composes self with a rotation that affects two
   * specified axes, replacing the current value of self.  The
   * rotation angle is in radians.  The axis of rotation goes
   * through the origin.  The transformation is given by
   *
   * y[axis1] =  std::cos(angle)*x[axis1] + std::sin(angle)*x[axis2]
   * y[axis2] = -sin(angle)*x[axis1] + std::cos(angle)*x[axis2].
   *
   * All coordinates other than axis1 and axis2 are unchanged;
   * a rotation of pi/2 radians will carry +axis1 into +axis2.
   * The rotation is precomposed with self if pre is true, and
   * postcomposed otherwise.
   * Note that the rotation is applied centered at the origin. */
  void
  Rotate(int axis1, int axis2, TParametersValueType angle, bool pre = false);

  /** Compose 2D affine transformation with a rotation
   *
   * This method composes self, which must be a 2D affine
   * transformation, with a clockwise rotation through a given angle
   * in radians.  The center of rotation is the origin.  The
   * rotation is precomposed with self if pre is true, and
   * postcomposed otherwise.
   * Note that the rotation is applied centered at the origin.
   *
   * \warning Only to be use in two dimensions
   *
   * \todo Find a way to generate a compile-time error
   *       is this is used with NDimensions != 2. */
  void
  Rotate2D(TParametersValueType angle, bool pre = false);

  /** Compose 3D affine transformation with a rotation
   *
   * This method composes self, which must be a 3D affine
   * transformation, with a clockwise rotation around a specified
   * axis.  The rotation angle is in radians; the axis of rotation
   * goes through the origin.  The rotation is precomposed with self
   * if pre is true, and postcomposed otherwise.
   * Note that the rotation is applied centered at the origin.
   *
   * \warning Only to be used in dimension 3
   *
   * \todo Find a way to generate a compile-time error
   * is this is used with NDimensions != 3. */
  void
  Rotate3D(const OutputVectorType & axis, TParametersValueType angle, bool pre = false);

  /** Compose affine transformation with a shear
   *
   * This method composes self with a shear transformation,
   * replacing the original contents of self.  The shear is
   * precomposed with self if pre is true, and postcomposed
   * otherwise.  The transformation is given by
   *
   * y[axis1] = x[axis1] + coef*x[axis2]
   * y[axis2] =                 x[axis2].
   *
   * Note that the shear is applied centered at the origin. */
  void
  Shear(int axis1, int axis2, TParametersValueType coef, bool pre = false);

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /** Compute distance between two affine transformations
   *
   * This method computes a "distance" between two affine
   * transformations.  This distance is guaranteed to be a metric,
   * but not any particular metric.  (At the moment, the algorithm
   * is to collect all the elements of the matrix and offset into a
   * vector, and compute the euclidean (L2) norm of that vector.
   * Some metric which could be used to estimate the distance between
   * two points transformed by the affine transformation would be
   * more useful, but I don't have time right now to work out the
   * mathematical details.) */
  ScalarType
  Metric(const Self * other) const;

  /** This method computes the distance from self to the identity
   * transformation, using the same metric as the one-argument form
   * of the Metric() method. */
  ScalarType
  Metric() const;

protected:
  /** Construct an AffineTransform object
   *
   * This method constructs a new AffineTransform object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the AffineTransform is initialized to an identity
   * transformation in the appropriate number of dimensions.   */
  AffineTransform(const MatrixType & matrix, const OutputVectorType & offset);
  AffineTransform(unsigned int parametersDimension);
  AffineTransform();

  /** Destroy an AffineTransform object   */
  ~AffineTransform() override = default;

  /** Print contents of an AffineTransform */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class AffineTransform

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAffineTransform.hxx"
#endif

#endif /* itkAffineTransform_h */
