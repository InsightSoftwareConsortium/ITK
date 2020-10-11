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
#ifndef itkIdentityTransform_h
#define itkIdentityTransform_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkArray2D.h"
#include "itkTransform.h"

namespace itk
{
/** \class IdentityTransform
 * \brief Implementation of an Identity Transform.
 *
 * This class defines the generic interface for an Identity Transform.
 *
 * It will map every point to itself, every vector to itself and
 * every covariant vector to itself.
 *
 * This class is intended to be used primarily as a default Transform
 * for initializing those classes supporting a generic Transform.
 *
 * This class is templated over the Representation type for coordinates
 * (that is the type used for representing the components of points and
 * vectors) and over the dimension of the space. In this case the Input
 * and Output spaces are the same so only one dimension is required.
 *
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT IdentityTransform : public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IdentityTransform);

  /** Standard class type aliases. */
  using Self = IdentityTransform;
  using Superclass = Transform<TParametersValueType, NDimensions, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IdentityTransform, Transform);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NDimensions;
  static constexpr unsigned int OutputSpaceDimension = NDimensions;

  /** Type of the input parameters. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using ScalarType = ParametersValueType;


  /** Type of the Jacobian matrix. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Transform category type. */
  using TransformCategoryEnum = typename Superclass::TransformCategoryEnum;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, Self::InputSpaceDimension>;
  using OutputVectorType = Vector<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard covariant vector type for this class */
  using InputCovariantVectorType = CovariantVector<TParametersValueType, Self::InputSpaceDimension>;
  using OutputCovariantVectorType = CovariantVector<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = vnl_vector_fixed<TParametersValueType, Self::InputSpaceDimension>;
  using OutputVnlVectorType = vnl_vector_fixed<TParametersValueType, Self::OutputSpaceDimension>;

  /** Standard coordinate point type for this class */
  using InputPointType = Point<TParametersValueType, Self::InputSpaceDimension>;
  using OutputPointType = Point<TParametersValueType, Self::OutputSpaceDimension>;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /**  Method to transform a point. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override
  {
    return point;
  }

  /**  Method to transform a vector. */
  using Superclass::TransformVector;
  OutputVectorType
  TransformVector(const InputVectorType & vector) const override
  {
    return vector;
  }

  /**  Method to transform a vnl_vector. */
  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vector) const override
  {
    return vector;
  }

  /**  Method to transform a CovariantVector. */
  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vector) const override
  {
    return vector;
  }

  /** Set the transformation to an Identity
   *
   * This is a nullptr operation in the case of this particular transform.
     The method is provided only to comply with the interface of other transforms. */
  void
  SetIdentity()
  {}

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point.
   *
   * The Jacobian can be expressed as a set of partial derivatives of the
   * output point components with respect to the parameters that defined
   * the transform:
   *
   * \f[
   *
      J=\left[ \begin{array}{cccc}
      \frac{\partial x_{1}}{\partial p_{1}} &
      \frac{\partial x_{2}}{\partial p_{1}} &
      \cdots  & \frac{\partial x_{n}}{\partial p_{1}}\\
      \frac{\partial x_{1}}{\partial p_{2}} &
      \frac{\partial x_{2}}{\partial p_{2}} &
      \cdots  & \frac{\partial x_{n}}{\partial p_{2}}\\
      \vdots  & \vdots  & \ddots  & \vdots \\
      \frac{\partial x_{1}}{\partial p_{m}} &
      \frac{\partial x_{2}}{\partial p_{m}} &
      \cdots  & \frac{\partial x_{n}}{\partial p_{m}}
      \end{array}\right]
   *
   * \f]
   */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType & jacobian) const override
  {
    jacobian = this->m_ZeroJacobian;
  }


  /** Get the jacobian with respect to position, which simply is an identity
   *  jacobian because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  void
  ComputeJacobianWithRespectToPosition(const InputPointType &, JacobianPositionType & jac) const override
  {
    jac.set_identity();
  }
  using Superclass::ComputeJacobianWithRespectToPosition;

  /* Always returns true if not null, as an identity is it's own inverse */
  bool
  GetInverse(Self * inverseTransform) const
  {
    return (inverseTransform != nullptr);
  }

  /** Return an inverse of the identity transform - another identity transform.
   */
  InverseTransformBasePointer
  GetInverseTransform() const override
  {
    return this->New().GetPointer();
  }

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   * \f[ T( a*P + b*Q ) = a * T(P) + b * T(Q) \f]
   */
  TransformCategoryEnum
  GetTransformCategory() const override
  {
    return Self::TransformCategoryEnum::Linear;
  }

  /** Get the Fixed Parameters. */
  const FixedParametersType &
  GetFixedParameters() const override
  {
    return this->m_FixedParameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  void
  SetFixedParameters(const FixedParametersType &) override
  {}

  /** Get the Parameters. */
  const ParametersType &
  GetParameters() const override
  {
    return this->m_Parameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  void
  SetParameters(const ParametersType &) override
  {}

protected:
  IdentityTransform()
    : Transform<TParametersValueType, NDimensions, NDimensions>(0)
    , m_ZeroJacobian(NDimensions, 0)
  {
    // The Jacobian is constant, therefore it can be initialized in the
    // constructor.
    this->m_ZeroJacobian.Fill(0.0);
  }

  ~IdentityTransform() override = default;

private:
  JacobianType m_ZeroJacobian;
};
} // end namespace itk

#endif
