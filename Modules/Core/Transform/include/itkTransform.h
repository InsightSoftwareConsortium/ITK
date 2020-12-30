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
#ifndef itkTransform_h
#define itkTransform_h

#include "itkTransformBase.h"
#include "itkVector.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkDiffusionTensor3D.h"
#include "itkVariableLengthVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMatrix.h"

namespace itk
{
/**
 *\class Transform
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
 * Subclasses must provide implementations for:<br>
 *   virtual OutputPointType           TransformPoint(const InputPointType  &) const<br>
 *   virtual OutputVectorType          TransformVector(const InputVectorType &) const<br>
 *   virtual OutputVnlVectorType       TransformVector(const InputVnlVectorType &) const<br>
 *   virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const<br>
 *   virtual void                      SetParameters(const ParametersType &)<br>
 *   virtual void                      SetFixedParameters(const FixedParametersType &)<br>
 *   virtual void                      ComputeJacobianWithRespectToParameters(
 *                                                             const InputPointType &,
 *                                                             JacobianType &) const<br>
 *   virtual void                      ComputeJacobianWithRespectToPosition(
 *                                                             const InputPointType & x,
 *                                                             JacobianPositionType &jacobian ) const;<br>
 *
 * Since TranformVector and TransformCovariantVector have multiple
 * overloaded methods from the base class, subclasses must specify:<br>
 *  using Superclass::TransformVector;<br>
 *  using Superclass::TransformCovariantVector;<br>
 *
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType, unsigned int NInputDimensions = 3, unsigned int NOutputDimensions = 3>
class ITK_TEMPLATE_EXPORT Transform : public TransformBaseTemplate<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Transform);

  /** Standard class type aliases. */
  using Self = Transform;
  using Superclass = TransformBaseTemplate<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Transform, TransformBaseTemplate);

  /** Dimension of the domain space. */
  static constexpr unsigned int InputSpaceDimension = NInputDimensions;
  static constexpr unsigned int OutputSpaceDimension = NOutputDimensions;

  /** define the Clone method */
  itkCloneMacro(Self);

  /** Get the size of the input space */
  unsigned int
  GetInputSpaceDimension() const override
  {
    return NInputDimensions;
  }

  /** Get the size of the output space */
  unsigned int
  GetOutputSpaceDimension() const override
  {
    return NOutputDimensions;
  }

  /** Type of the input parameters. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using DerivativeType = Array<ParametersValueType>;

  /** Type of the scalar representing coordinate and vector elements. */
  using ScalarType = ParametersValueType;

  /** Type of the Jacobian matrix. */
  using JacobianType = Array2D<ParametersValueType>;
  using JacobianPositionType = vnl_matrix_fixed<ParametersValueType, NOutputDimensions, NInputDimensions>;
  using InverseJacobianPositionType = vnl_matrix_fixed<ParametersValueType, NInputDimensions, NOutputDimensions>;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, NInputDimensions>;
  using OutputVectorType = Vector<TParametersValueType, NOutputDimensions>;

  /** Standard variable length vector type for this class
   *  this provides an interface for the VectorImage class */
  using InputVectorPixelType = VariableLengthVector<TParametersValueType>;
  using OutputVectorPixelType = VariableLengthVector<TParametersValueType>;

  /* Standard symmetric second rank tenosr type for this class */
  using InputSymmetricSecondRankTensorType = SymmetricSecondRankTensor<TParametersValueType, NInputDimensions>;
  using OutputSymmetricSecondRankTensorType = SymmetricSecondRankTensor<TParametersValueType, NOutputDimensions>;

  /* Standard tensor type for this class */
  using InputDiffusionTensor3DType = DiffusionTensor3D<TParametersValueType>;
  using OutputDiffusionTensor3DType = DiffusionTensor3D<TParametersValueType>;

  /** Standard covariant vector type for this class */
  using InputCovariantVectorType = CovariantVector<TParametersValueType, NInputDimensions>;
  using OutputCovariantVectorType = CovariantVector<TParametersValueType, NOutputDimensions>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = vnl_vector_fixed<TParametersValueType, NInputDimensions>;
  using OutputVnlVectorType = vnl_vector_fixed<TParametersValueType, NOutputDimensions>;

  /** Standard coordinate point type for this class */
  using InputPointType = Point<TParametersValueType, NInputDimensions>;
  using OutputPointType = Point<TParametersValueType, NOutputDimensions>;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = Transform<TParametersValueType, NOutputDimensions, NInputDimensions>;

  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  using MatrixType = Matrix<TParametersValueType, Self::OutputSpaceDimension, Self::InputSpaceDimension>;

  using OutputDirectionMatrix = Matrix<double, Self::OutputSpaceDimension, Self::OutputSpaceDimension>;
  using InputDirectionMatrix = Matrix<double, Self::InputSpaceDimension, Self::InputSpaceDimension>;
  using DirectionChangeMatrix = Matrix<double, Self::OutputSpaceDimension, Self::InputSpaceDimension>;

  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /**  Method to transform a point.
   * \warning This method must be thread-safe. See, e.g., its use
   * in ResampleImageFilter.
   */
  virtual OutputPointType
  TransformPoint(const InputPointType &) const = 0;

  /**  Method to transform a vector. */
  virtual OutputVectorType
  TransformVector(const InputVectorType &) const
  {
    itkExceptionMacro("TransformVector(const InputVectorType &)"
                      "is unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a vector at a given location.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVectorType
  TransformVector(const InputVectorType & vector, const InputPointType & point) const;

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType
  TransformVector(const InputVnlVectorType &) const
  {
    itkExceptionMacro("TransformVector( const InputVnlVectorType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a vnl_vector, at a point.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVnlVectorType
  TransformVector(const InputVnlVectorType & vector, const InputPointType & point) const;

  /** Method to transform a vector stored in a VectorImage.  */
  virtual OutputVectorPixelType
  TransformVector(const InputVectorPixelType & itkNotUsed(vector)) const
  {
    itkExceptionMacro("TransformVector( const InputVectorPixelType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a vector stored in a VectorImage, at a point.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVectorPixelType
  TransformVector(const InputVectorPixelType & vector, const InputPointType & point) const;

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const
  {
    itkExceptionMacro("TransformCovariantVector( const InputCovariantVectorType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }
  /** Method to transform a CovariantVector, using a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and
   * \c TransformCovariantVector(vector) is called */
  virtual OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & vector, const InputPointType & point) const;


  /**  Method to transform a CovariantVector stored in a VectorImage. */
  virtual OutputVectorPixelType
  TransformCovariantVector(const InputVectorPixelType & itkNotUsed(vector)) const
  {
    itkExceptionMacro("TransformCovariantVector(const InputVectorPixelType &)"
                      "is unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a CovariantVector, using a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformCovariantVector(vector) is
   * called */
  virtual OutputVectorPixelType
  TransformCovariantVector(const InputVectorPixelType & vector, const InputPointType & point) const;

  /** Method to transform a diffusion tensor */
  virtual OutputDiffusionTensor3DType
  TransformDiffusionTensor3D(const InputDiffusionTensor3DType & itkNotUsed(tensor)) const
  {
    itkExceptionMacro("TransformDiffusionTensor3D( const InputDiffusionTensor3DType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a diffusion tensor at a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformDiffusionTensor(tensor) is
   * called */
  virtual OutputDiffusionTensor3DType
  TransformDiffusionTensor3D(const InputDiffusionTensor3DType & inputTensor, const InputPointType & point) const;

  /** Method to transform a diffusion tensor stored in a VectorImage */
  virtual OutputVectorPixelType
  TransformDiffusionTensor3D(const InputVectorPixelType & itkNotUsed(tensor)) const
  {
    itkExceptionMacro("TransformDiffusionTensor( const InputVectorPixelType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  virtual OutputVectorPixelType
  TransformDiffusionTensor3D(const InputVectorPixelType & inputTensor, const InputPointType & point) const;

  /** Method to transform a diffusion tensor at a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformSymmetricSecondRankTensor(tensor) is
   * called */
  virtual OutputSymmetricSecondRankTensorType
  TransformSymmetricSecondRankTensor(const InputSymmetricSecondRankTensorType & inputTensor,
                                     const InputPointType &                     point) const;

  /** Method to transform a ssr tensor stored in a VectorImage */
  virtual OutputSymmetricSecondRankTensorType
  TransformSymmetricSecondRankTensor(const InputSymmetricSecondRankTensorType & itkNotUsed(tensor)) const
  {
    itkExceptionMacro("TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a ssr tensor stored in a VectorImage */
  virtual OutputVectorPixelType
  TransformSymmetricSecondRankTensor(const InputVectorPixelType & itkNotUsed(tensor)) const
  {
    itkExceptionMacro("TransformSymmetricSecondRankTensor( const InputVectorPixelType & ) is "
                      "unimplemented for "
                      << this->GetNameOfClass());
  }

  /** Method to transform a diffusion tensor stored in a VectorImage, at
   * a point.  Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformDiffusionTensor(tensor) is
   * called */
  virtual OutputVectorPixelType
  TransformSymmetricSecondRankTensor(const InputVectorPixelType & inputTensor, const InputPointType & point) const;

  /** Set the transformation parameters and update internal transformation.
   * SetParameters gives the transform the option to set it's
   * parameters by keeping a reference to the parameters, or by
   * copying.  To force the transform to copy it's parameters call
   * SetParametersByValue.
   * \sa SetParametersByValue
   */
  void
  SetParameters(const ParametersType &) override = 0;

  /** Set the transformation parameters and update internal transformation.
   * This method forces the transform to copy the parameters.  The
   * default implementation is to call SetParameters.  This call must
   * be overridden if the transform normally implements SetParameters
   * by keeping a reference to the parameters.
   * \sa SetParameters
   */
  void
  SetParametersByValue(const ParametersType & p) override
  {
    this->SetParameters(p);
  }

  /** This function allow copying a range of values into the Parameters
   * The range of values must conform to std::copy(begin, end, m_Parameters)
   * requirements.
   */
  void
  CopyInParameters(const ParametersValueType * const begin, const ParametersValueType * const end) override;

  /** This function allow copying a range of values into the FixedParameters
   * The range of values must conform to std::copy(begin, end, m_FixedParameters)
   * requirements.
   */
  void
  CopyInFixedParameters(const FixedParametersValueType * const begin,
                        const FixedParametersValueType * const end) override;

  /** Get the Transformation Parameters. */
  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  void
  SetFixedParameters(const FixedParametersType &) override = 0;

  /** Get the Fixed Parameters. */
  const FixedParametersType &
  GetFixedParameters() const override
  {
    return m_FixedParameters;
  }

  /** Update the transform's parameters by the values in \c update.
   * \param update must be of the same length as returned by
   * GetNumberOfParameters(). Throw an exception otherwise.
   * \param factor is a scalar multiplier for each value in \c update.
   * SetParameters is called at the end of this method, to allow the transform
   * to perform any required operations on the updated parameters - typically
   * a conversion to member variables for use in TransformPoint. */
  virtual void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType factor = 1.0);

  /** Return the number of local parameters that completely defines the
   *  Transform at an individual voxel.
   *  For transforms with local support, this will enable downstream
   *  computation of the jacobian wrt only the local support region.
   *  For instance, in the case of a deformation field, this will be equal to
   *  the number of image dimensions. If it is an affine transform, this will
   *  be the same as the GetNumberOfParameters().
   */
  virtual NumberOfParametersType
  GetNumberOfLocalParameters() const
  {
    return this->GetNumberOfParameters();
  }

  /** Return the number of parameters that completely define the Transfom  */
  NumberOfParametersType
  GetNumberOfParameters() const override
  {
    return this->m_Parameters.Size();
  }

  /** Return the number of parameters that define the constant elements of a Transfom  */
  virtual NumberOfParametersType
  GetNumberOfFixedParameters() const
  {
    return this->m_FixedParameters.Size();
  }

  /** Returns a boolean indicating whether it is possible or not to compute the
   * inverse of this current Transform. If it is possible, then the inverse of
   * the transform is returned in the inverseTransform variable passed by the
   * user.  The inverse is recomputed if this current transform has been
   * modified.
   * This method is intended to be overriden as needed by derived classes.
   *
   */
  bool
  GetInverse(Self * itkNotUsed(inverseTransform)) const
  {
    return false;
  }

  /** Return an inverse of this transform. If the inverse has not been
   *  implemented, return nullptr. The type of the inverse transform
   *  does not necessarily need to match the type of the forward
   *  transform. This allows one to return a numeric inverse transform
   *  instead.
   */
  virtual InverseTransformBasePointer
  GetInverseTransform() const
  {
    return nullptr;
  }

  /** Generate a platform independent name */
  std::string
  GetTransformTypeAsString() const override;

  using TransformCategoryEnum = typename Superclass::TransformCategoryEnum;

  /** Indicates the category transform.
   *  e.g. an affine transform, or a local one, e.g. a deformation field.
   */
  TransformCategoryEnum
  GetTransformCategory() const override
  {
    return Superclass::TransformCategoryEnum::UnknownTransformCategory;
  }

  virtual bool
  IsLinear() const
  {
    return (this->GetTransformCategory() == Superclass::TransformCategoryEnum::Linear);
  }

  /**
   * Compute the Jacobian of the transformation
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
   *
   *  This is also used for efficient computation of a point-local jacobian
   *  for dense transforms.
   *  \c jacobian is assumed to be thread-local variable, otherwise memory corruption
   *  will most likely occur during multi-threading.
   *  To avoid repetitive memory allocation, pass in 'jacobian' with its size
   *  already set. */
  virtual void
  ComputeJacobianWithRespectToParameters(const InputPointType & itkNotUsed(p),
                                         JacobianType &         itkNotUsed(jacobian)) const = 0;

  virtual void
  ComputeJacobianWithRespectToParametersCachedTemporaries(const InputPointType & p,
                                                          JacobianType &         jacobian,
                                                          JacobianType &         itkNotUsed(cachedJacobian)) const
  {
    // NOTE: default implementation is not optimized, and just falls back to original methods.
    this->ComputeJacobianWithRespectToParameters(p, jacobian);
  }


  /** This provides the ability to get a local jacobian value
   *  in a dense/local transform, e.g. DisplacementFieldTransform. For such
   *  transforms it would be unclear what parameters would refer to.
   *  Generally, global transforms should return an identity jacobian
   *  since there is no change with respect to position. */
  virtual void
  ComputeJacobianWithRespectToPosition(const InputPointType & itkNotUsed(x),
                                       JacobianPositionType & itkNotUsed(jacobian)) const
  {
    itkExceptionMacro("ComputeJacobianWithRespectToPosition( InputPointType, JacobianType )"
                      " is unimplemented for "
                      << this->GetNameOfClass());
  }
  itkLegacyMacro(virtual void ComputeJacobianWithRespectToPosition(const InputPointType & x, JacobianType & jacobian)
                   const);


  /** This provides the ability to get a local jacobian value
   *  in a dense/local transform, e.g. DisplacementFieldTransform. For such
   *  transforms it would be unclear what parameters would refer to.
   *  Generally, global transforms should return an identity jacobian
   *  since there is no change with respect to position. */
  virtual void
  ComputeInverseJacobianWithRespectToPosition(const InputPointType & pnt, InverseJacobianPositionType & jacobian) const;
  itkLegacyMacro(virtual void ComputeInverseJacobianWithRespectToPosition(const InputPointType & x,
                                                                          JacobianType &         jacobian) const);

protected:
  /**
   * Clone the current transform.
   * This does a complete copy of the transform
   * state to the new transform
   */
  typename LightObject::Pointer
  InternalClone() const override;

  Transform();
  Transform(NumberOfParametersType numberOfParameters);
#if defined(__GNUC__)
  // A bug in some versions of the GCC and Clang compilers
  // result in an ICE or linker error when "= default" is requested.
  // This was observed in at least gcc 4.8 and 5.4.0, and
  // AppleClang 7.0.2 and 8.0.0. Probably others too.
  // "= default" doesn't gain us much, so just don't use it here.
  ~Transform() override{};
#else
  ~Transform() override = default;
#endif
  mutable ParametersType      m_Parameters;
  mutable FixedParametersType m_FixedParameters;

  OutputDiffusionTensor3DType
  PreservationOfPrincipalDirectionDiffusionTensor3DReorientation(const InputDiffusionTensor3DType &,
                                                                 const InverseJacobianPositionType &) const;

  mutable DirectionChangeMatrix m_DirectionChange;

private:
  template <typename TType>
  static std::string
  GetTransformTypeAsString(TType *)
  {
    std::string rval("other");

    return rval;
  }

  static std::string
  GetTransformTypeAsString(float *)
  {
    std::string rval("float");

    return rval;
  }

  static std::string
  GetTransformTypeAsString(double *)
  {
    std::string rval("double");

    return rval;
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTransform.hxx"
#endif

#endif
