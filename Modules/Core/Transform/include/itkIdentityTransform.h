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
template<typename TParametersValueType,
          unsigned int NDimensions=3>
class ITK_TEMPLATE_EXPORT IdentityTransform : public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef IdentityTransform                                           Self;
  typedef Transform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IdentityTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NDimensions);

  /** Type of the input parameters. */
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef ParametersValueType                           ScalarType;


  /** Type of the Jacobian matrix. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType,
                 itkGetStaticConstMacro(InputSpaceDimension)>  InputVectorType;
  typedef Vector<TParametersValueType,
                 itkGetStaticConstMacro(OutputSpaceDimension)> OutputVectorType;

  /** Standard covariant vector type for this class */
  typedef CovariantVector<TParametersValueType,
                          itkGetStaticConstMacro(InputSpaceDimension)>  InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType,
                          itkGetStaticConstMacro(OutputSpaceDimension)> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType,
                           itkGetStaticConstMacro(InputSpaceDimension)>  InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType,
                           itkGetStaticConstMacro(OutputSpaceDimension)> OutputVnlVectorType;

  /** Standard coordinate point type for this class */
  typedef Point<TParametersValueType,
                itkGetStaticConstMacro(InputSpaceDimension)> InputPointType;
  typedef Point<TParametersValueType,
                itkGetStaticConstMacro(OutputSpaceDimension)> OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /**  Method to transform a point. */
  virtual OutputPointType TransformPoint(const InputPointType  & point) const ITK_OVERRIDE
  {
    return point;
  }

  /**  Method to transform a vector. */
  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType & vector) const ITK_OVERRIDE
  {
    return vector;
  }

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType & vector) const ITK_OVERRIDE
  {
    return vector;
  }

  /**  Method to transform a CovariantVector. */
  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType & vector) const ITK_OVERRIDE
  {
    return vector;
  }

  /** Set the transformation to an Identity
   *
   * This is a ITK_NULLPTR operation in the case of this particular transform.
     The method is provided only to comply with the interface of other transforms. */
  void SetIdentity(void)
  {
  }

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
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType &,
                                                       JacobianType & jacobian) const ITK_OVERRIDE
  {
    jacobian = this->m_IdentityJacobian;
  }

  /** Get the jacobian with respect to position, which simply is an identity
   *  jacobian because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType &,
                                                    JacobianType & jac) const ITK_OVERRIDE
  {
    jac.SetSize( NDimensions, NDimensions );
    jac.Fill(0.0);
    for( unsigned int dim = 0; dim < NDimensions; dim++ )
      {
      jac[dim][dim] = 1.0;
      }
  }

  /* Always returns true if not null, as an identity is it's own inverse */
  bool GetInverse( Self *inverseTransform ) const
  {
    return (inverseTransform != ITK_NULLPTR);
  }

  /** Return an inverse of the identity transform - another identity transform.
    */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE
  {
    return this->New().GetPointer();
  }

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   * \f[ T( a*P + b*Q ) = a * T(P) + b * T(Q) \f]
   */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Self::Linear;
  }

  /** Get the Fixed Parameters. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE
  {
    return this->m_FixedParameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE
  {
  }

  /** Get the Parameters. */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  {
    return this->m_Parameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetParameters(const ParametersType &) ITK_OVERRIDE
  {
  }

protected:
  IdentityTransform() : Transform<TParametersValueType, NDimensions, NDimensions>(0),
    m_IdentityJacobian(NDimensions, 0)
  {
    // The Jacobian is constant, therefore it can be initialized in the
    // constructor.
    this->m_IdentityJacobian.Fill(0.0);
  }

  virtual ~IdentityTransform() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IdentityTransform);

  JacobianType m_IdentityJacobian;
};
} // end namespace itk

#endif
