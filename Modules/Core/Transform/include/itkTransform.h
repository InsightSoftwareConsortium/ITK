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
#ifndef __itkTransform_h
#define __itkTransform_h

#include "itkTransformBase.h"
#include "itkVector.h"
#include "vnl/vnl_vector_fixed.h"


namespace itk
{
/** \class Transform
 * \brief Transform points and vectors from an input space to an output space.
 *
 * This abstract class defines the generic interface for a geometric
 * transformation from one space to another. The class provides methods
 * for mapping points, vectors and covariant vectors from the input space
 * to the output space.
 *
 * Given that transformations are not necessarily invertible, this basic
 * class does not provide the methods for back transformation. Back transform
 * methods are implemented in derived classes where appropriate.
 *
 * \par Registration Framework Support
 * Typically a Transform class has several methods for setting its
 * parameters. For use in the registration framework, the parameters must
 * also be represented by an array of doubles to allow communication
 * with generic optimizers. The Array of transformation parameters is set using
 * the SetParameters() method.
 *
 * Another requirement of the registration framework is the computation
 * of the transform Jacobian. In general, an ImageToImageMetric requires
 * the knowledge of the Jacobian in order to compute the metric derivatives.
 * The Jacobian is a matrix whose element are the partial derivatives
 * of the output point with respect to the array of parameters that defines
 * the transform.
 *
 * Subclasses must provide implementations for:
 *   OutputPointType           TransformPoint(const InputPointType  &) const
 *   OutputVectorType          TransformVector(const InputVectorType &) const
 *   OutputVnlVectorType       TransformVector(const InputVnlVectorType &) const
 *   OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const
 *   void                      SetParameters(const ParametersType &)
 *   void                      SetFixedParameters(const ParametersType &)
 *   const                     JacobianType & GetJacobian(const InputPointType  &) const
 * \ingroup Transforms
 *
 * \ingroup ITKTransform
 */
template< class TScalarType,
          unsigned int NInputDimensions = 3,
          unsigned int NOutputDimensions = 3 >
class ITK_EXPORT Transform:public TransformBase
{
public:
  /** Standard class typedefs. */
  typedef Transform                  Self;
  typedef TransformBase              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Transform, TransformBase);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NInputDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NOutputDimensions);

  /** Get the size of the input space */
  unsigned int GetInputSpaceDimension(void) const { return NInputDimensions; }

  /** Get the size of the output space */
  unsigned int GetOutputSpaceDimension(void) const { return NOutputDimensions; }

  /** Type of the scalar representing coordinate and vector elements. */
  typedef  TScalarType ScalarType;

  /** Type of the input parameters. */
  typedef  typename Superclass::ParametersType      ParametersType;
  typedef  typename Superclass::ParametersValueType ParametersValueType;

  /** Type of the Jacobian matrix. */
  typedef  Array2D< double > JacobianType;

  /** Standard vector type for this class. */
  typedef Vector< TScalarType, NInputDimensions >  InputVectorType;
  typedef Vector< TScalarType, NOutputDimensions > OutputVectorType;

  /** Standard covariant vector type for this class */
  typedef CovariantVector< TScalarType, NInputDimensions >  InputCovariantVectorType;
  typedef CovariantVector< TScalarType, NOutputDimensions > OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed< TScalarType, NInputDimensions >  InputVnlVectorType;
  typedef vnl_vector_fixed< TScalarType, NOutputDimensions > OutputVnlVectorType;

  /** Standard coordinate point type for this class */
  typedef Point< TScalarType, NInputDimensions >  InputPointType;
  typedef Point< TScalarType, NOutputDimensions > OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef Transform<
    TScalarType, NOutputDimensions, NInputDimensions > InverseTransformBaseType;

  typedef typename InverseTransformBaseType::Pointer InverseTransformBasePointer;

  /**  Method to transform a point.
   * \warning This method must be thread-safe. See, e.g., its use
   * in ResampleImageFilter.
   */
  virtual OutputPointType TransformPoint(const InputPointType  &) const = 0;

  /**  Method to transform a vector. */
  virtual OutputVectorType    TransformVector(const InputVectorType &) const = 0;

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const = 0;

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const = 0;

  /** Set the transformation parameters and update internal transformation.
   * SetParameters gives the transform the option to set it's
   * parameters by keeping a reference to the parameters, or by
   * copying.  To force the transform to copy it's parameters call
   * SetParametersByValue.
   * \sa SetParametersByValue
   */
  virtual void SetParameters(const ParametersType &) = 0;

  /** Set the transformation parameters and update internal transformation.
   * This method forces the transform to copy the parameters.  The
   * default implementation is to call SetParameters.  This call must
   * be overridden if the transform normally implements SetParameters
   * by keeping a reference to the parameters.
   * \sa SetParameters
   */
  virtual void SetParametersByValue(const ParametersType & p)
  { this->SetParameters (p); }

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters(void) const
  {
    return m_Parameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters(const ParametersType &) = 0;

  /** Get the Fixed Parameters. */
  virtual const ParametersType & GetFixedParameters(void) const
  {
    return m_FixedParameters;
  }

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation
   * at a given input point. The rank of the Jacobian will also indicate
   * if the transform is invertible at this point.
   *
   * The Jacobian is be expressed as a matrix of partial derivatives of the
   * output point components with respect to the parameters that defined
   * the transform:
   *
   * \f[
   *
  J=\left[ \begin{array}{cccc}
  \frac{\partial x_{1}}{\partial p_{1}} &
  \frac{\partial x_{1}}{\partial p_{2}} &
  \cdots  & \frac{\partial x_{1}}{\partial p_{m}}\\
  \frac{\partial x_{2}}{\partial p_{1}} &
  \frac{\partial x_{2}}{\partial p_{2}} &
  \cdots  & \frac{\partial x_{2}}{\partial p_{m}}\\
  \vdots  & \vdots  & \ddots  & \vdots \\
  \frac{\partial x_{n}}{\partial p_{1}} &
  \frac{\partial x_{n}}{\partial p_{2}} &
  \cdots  & \frac{\partial x_{n}}{\partial p_{m}}
  \end{array}\right]
   *
   * \f]
   * */
  virtual const JacobianType & GetJacobian(const InputPointType  &) const = 0;

  /** Return the number of parameters that completely define the Transfom  */
  virtual unsigned int GetNumberOfParameters(void) const
  { return this->m_Parameters.Size(); }

  /** Returns a boolean indicating whether it is possible or not to compute the
   * inverse of this current Transform. If it is possible, then the inverse of
   * the transform is returned in the inverseTransform variable passed by the
   * user.  The inverse is recomputed if this current transform has been modified.
   * This method is intended to be overriden by derived classes.
   *
   */
  bool GetInverse( Self *itkNotUsed(inverseTransform) ) const { return false; }

  /** Return an inverse of this transform. If the inverse has not been
   *  implemented, return NULL. The type of the inverse transform
   *  does not necessarily need to match the type of the forward
   *  transform. This allows one to return a numeric inverse transform
   *  instead.
   */
  virtual InverseTransformBasePointer GetInverseTransform() const { return NULL; }

  /** Generate a platform independant name */
  virtual std::string GetTransformTypeAsString() const;

  /** Indicates if this transform is linear. A transform is defined to be
   * linear if the transform of a linear combination of points is equal to the
   * linear combination (with the same coefficients) of the individual
   * transforms of each point. The transform T will be linear if given two
   * points P and Q, and scalar coefficients a and b, then
   *
   *           T( a*P + b*Q ) = a * T(P) + b * T(Q)
   *
   * By default, we assume this to NOT be the case for most transforms.
   * However, transforms for which this is true will overload and reimplement
   * this method accordingly.
   *
   * \warning This method must be thread-safe. See, e.g., its use
   * in ResampleImageFilter.
   */
  virtual bool IsLinear() const { return false; }
protected:
  Transform();
  Transform(unsigned int Dimension, unsigned int NumberOfParameters);
  virtual ~Transform() {}

  mutable ParametersType m_Parameters;
  mutable ParametersType m_FixedParameters;

  mutable JacobianType m_Jacobian;
private:
  Transform(const Self &);      //purposely not implemented
  void operator=(const Self &); //purposely not implemented
  template <typename TType>
    std::string GetTransformTypeAsString(TType *) const
  {
    std::string rval("other");
    return rval;
  }
  std::string GetTransformTypeAsString(float *) const
  {
    std::string rval("float");
    return rval;
  }
  std::string GetTransformTypeAsString(double *) const
  {
    std::string rval("double");
    return rval;
  }
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_Transform(_, EXPORT, TypeX, TypeY)         \
  namespace itk                                                 \
  {                                                             \
  _( 3 ( class EXPORT Transform< ITK_TEMPLATE_3 TypeX > ) )     \
  namespace Templates                                           \
  {                                                             \
  typedef Transform< ITK_TEMPLATE_3 TypeX > Transform##TypeY; \
  }                                                             \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkTransform.hxx"
#endif

#endif
