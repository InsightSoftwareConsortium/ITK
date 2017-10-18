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
#ifndef itkTransform_h
#define itkTransform_h

#include "itkTransformBase.h"
#include "itkVector.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkDiffusionTensor3D.h"
#include "itkVariableLengthVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkMatrix.h"

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
 *                                                             JacobianType &jacobian ) const;<br>
 *
 * Since TranformVector and TransformCovariantVector have multiple
 * overloaded methods from the base class, subclasses must specify:<br>
 *  using Superclass::TransformVector;<br>
 *  using Superclass::TransformCovariantVector;<br>
 *
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType,
          unsigned int NInputDimensions = 3,
          unsigned int NOutputDimensions = 3>
class ITK_TEMPLATE_EXPORT Transform : public TransformBaseTemplate<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef Transform                                   Self;
  typedef TransformBaseTemplate<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Transform, TransformBaseTemplate);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NInputDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NOutputDimensions);

  /** define the Clone method */
  itkCloneMacro(Self);

  /** Get the size of the input space */
  unsigned int GetInputSpaceDimension(void) const ITK_OVERRIDE
  {
    return NInputDimensions;
  }

  /** Get the size of the output space */
  unsigned int GetOutputSpaceDimension(void) const ITK_OVERRIDE
  {
    return NOutputDimensions;
  }

  /** Type of the input parameters. */
  typedef  typename Superclass::FixedParametersType      FixedParametersType;
  typedef  typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef  typename Superclass::ParametersType           ParametersType;
  typedef  typename Superclass::ParametersValueType      ParametersValueType;
  typedef  Array<ParametersValueType>                    DerivativeType;

  /** Type of the scalar representing coordinate and vector elements. */
  typedef  ParametersValueType ScalarType;

  /** Type of the Jacobian matrix. */
  typedef  Array2D<ParametersValueType> JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType, NInputDimensions>  InputVectorType;
  typedef Vector<TParametersValueType, NOutputDimensions> OutputVectorType;

  /** Standard variable length vector type for this class
   *  this provides an interface for the VectorImage class */
  typedef VariableLengthVector<TParametersValueType> InputVectorPixelType;
  typedef VariableLengthVector<TParametersValueType> OutputVectorPixelType;

  /* Standard symmetric second rank tenosr type for this class */
  typedef SymmetricSecondRankTensor<TParametersValueType,NInputDimensions>
    InputSymmetricSecondRankTensorType;
  typedef SymmetricSecondRankTensor<TParametersValueType,NOutputDimensions>
    OutputSymmetricSecondRankTensorType;

  /* Standard tensor type for this class */
  typedef DiffusionTensor3D<TParametersValueType> InputDiffusionTensor3DType;
  typedef DiffusionTensor3D<TParametersValueType> OutputDiffusionTensor3DType;

  /** Standard covariant vector type for this class */
  typedef CovariantVector<TParametersValueType, NInputDimensions>
  InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType, NOutputDimensions>
  OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType, NInputDimensions>  InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType, NOutputDimensions> OutputVnlVectorType;

  /** Standard coordinate point type for this class */
  typedef Point<TParametersValueType, NInputDimensions>  InputPointType;
  typedef Point<TParametersValueType, NOutputDimensions> OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef Transform<TParametersValueType,
                     NOutputDimensions, NInputDimensions> InverseTransformBaseType;

  typedef typename InverseTransformBaseType::Pointer InverseTransformBasePointer;

  typedef Matrix<TParametersValueType,
                 itkGetStaticConstMacro(OutputSpaceDimension),
                 itkGetStaticConstMacro(InputSpaceDimension)>     MatrixType;

  typedef Matrix<double,
                 itkGetStaticConstMacro(OutputSpaceDimension),
                 itkGetStaticConstMacro(OutputSpaceDimension)>
  OutputDirectionMatrix;
  typedef Matrix<double,
                 itkGetStaticConstMacro(InputSpaceDimension),
                 itkGetStaticConstMacro(InputSpaceDimension)>
  InputDirectionMatrix;
  typedef Matrix<double,
                 itkGetStaticConstMacro(OutputSpaceDimension),
                 itkGetStaticConstMacro(InputSpaceDimension)>
  DirectionChangeMatrix;

  typedef typename Superclass::NumberOfParametersType    NumberOfParametersType;

  /**  Method to transform a point.
   * \warning This method must be thread-safe. See, e.g., its use
   * in ResampleImageFilter.
   */
  virtual OutputPointType TransformPoint(const InputPointType  &) const = 0;

  /**  Method to transform a vector. */
  virtual OutputVectorType  TransformVector(const InputVectorType &) const
  {
    itkExceptionMacro( "TransformVector(const InputVectorType &)"
                       "is unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a vector at a given location.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVectorType    TransformVector(
    const InputVectorType & vector,
    const InputPointType & point ) const;

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const
  {
    itkExceptionMacro( "TransformVector( const InputVnlVectorType & ) is "
                       "unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a vnl_vector, at a point.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVnlVectorType TransformVector(
    const InputVnlVectorType & vector,
    const InputPointType & point ) const;

  /** Method to transform a vector stored in a VectorImage.  */
  virtual OutputVectorPixelType TransformVector(
    const InputVectorPixelType & itkNotUsed(vector) ) const
  {
    itkExceptionMacro( "TransformVector( const InputVectorPixelType & ) is "
                       "unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a vector stored in a VectorImage, at a point.
   * For global transforms, \c point is ignored and \c TransformVector( vector )
   * is called. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior. */
  virtual OutputVectorPixelType TransformVector(
    const InputVectorPixelType & vector,
    const InputPointType & point ) const;

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const
  {
    itkExceptionMacro( "TransformCovariantVector( const InputCovariantVectorType & ) is "
                       "unimplemented for " << this->GetNameOfClass() );
  }
  /** Method to transform a CovariantVector, using a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and
   * \c TransformCovariantVector(vector) is called */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType & vector,
    const InputPointType & point ) const;


  /**  Method to transform a CovariantVector stored in a VectorImage. */
  virtual OutputVectorPixelType TransformCovariantVector(
    const InputVectorPixelType & itkNotUsed(vector) ) const
  {
    itkExceptionMacro( "TransformCovariantVector(const InputVectorPixelType &)"
                       "is unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a CovariantVector, using a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformCovariantVector(vector) is
   * called */
  virtual OutputVectorPixelType TransformCovariantVector(
    const InputVectorPixelType & vector,
    const InputPointType & point ) const;

  /** Method to transform a diffusion tensor */
  virtual OutputDiffusionTensor3DType TransformDiffusionTensor3D(
    const InputDiffusionTensor3DType & itkNotUsed(tensor) )
  const
  {
    itkExceptionMacro(
      "TransformDiffusionTensor3D( const InputDiffusionTensor3DType & ) is "
      "unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a diffusion tensor at a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformDiffusionTensor(tensor) is
   * called */
  virtual OutputDiffusionTensor3DType TransformDiffusionTensor3D(
    const InputDiffusionTensor3DType & tensor,
    const InputPointType & point ) const;

  /** Method to transform a diffusion tensor stored in a VectorImage */
  virtual OutputVectorPixelType TransformDiffusionTensor3D(
    const InputVectorPixelType & itkNotUsed(tensor) ) const
  {
    itkExceptionMacro(
      "TransformDiffusionTensor( const InputVectorPixelType & ) is "
      "unimplemented for " << this->GetNameOfClass() );
  }

  virtual OutputVectorPixelType TransformDiffusionTensor3D(
    const InputVectorPixelType & tensor,
    const InputPointType & point ) const;

  /** Method to transform a diffusion tensor at a point. Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformSymmetricSecondRankTensor(tensor) is
   * called */
  virtual OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor(
    const InputSymmetricSecondRankTensorType & tensor,
    const InputPointType & point ) const;

  /** Method to transform a ssr tensor stored in a VectorImage */
  virtual OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor(
    const InputSymmetricSecondRankTensorType & itkNotUsed(tensor) ) const
  {
    itkExceptionMacro(
      "TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType & ) is "
      "unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a ssr tensor stored in a VectorImage */
  virtual OutputVectorPixelType TransformSymmetricSecondRankTensor(
    const InputVectorPixelType & itkNotUsed(tensor) ) const
  {
    itkExceptionMacro(
      "TransformSymmetricSecondRankTensor( const InputVectorPixelType & ) is "
      "unimplemented for " << this->GetNameOfClass() );
  }

  /** Method to transform a diffusion tensor stored in a VectorImage, at
   * a point.  Global transforms
   * can ignore the \c point parameter. Local transforms (e.g. deformation
   * field transform) must override and provide required behavior.
   * By default, \c point is ignored and \c TransformDiffusionTensor(tensor) is
   * called */
  virtual OutputVectorPixelType TransformSymmetricSecondRankTensor(
    const InputVectorPixelType & tensor,
    const InputPointType & point ) const;

  /** Set the transformation parameters and update internal transformation.
   * SetParameters gives the transform the option to set it's
   * parameters by keeping a reference to the parameters, or by
   * copying.  To force the transform to copy it's parameters call
   * SetParametersByValue.
   * \sa SetParametersByValue
   */
  virtual void SetParameters(const ParametersType &) ITK_OVERRIDE = 0;

  /** Set the transformation parameters and update internal transformation.
   * This method forces the transform to copy the parameters.  The
   * default implementation is to call SetParameters.  This call must
   * be overridden if the transform normally implements SetParameters
   * by keeping a reference to the parameters.
   * \sa SetParameters
   */
  virtual void SetParametersByValue(const ParametersType & p) ITK_OVERRIDE
  {
    this->SetParameters(p);
  }

  /** This function allow copying a range of values into the Parameters
    * The range of values must conform to std::copy(begin, end, m_Parameters)
    * requirements.
    */
  virtual void CopyInParameters(const ParametersValueType * const begin,
                                const ParametersValueType * const end) ITK_OVERRIDE;

  /** This function allow copying a range of values into the FixedParameters
    * The range of values must conform to std::copy(begin, end, m_FixedParameters)
    * requirements.
    */
  virtual void CopyInFixedParameters(const FixedParametersValueType * const begin,
                                     const FixedParametersValueType * const end) ITK_OVERRIDE;

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters(void) const ITK_OVERRIDE
  {
    return m_Parameters;
  }

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE = 0;

  /** Get the Fixed Parameters. */
  virtual const FixedParametersType & GetFixedParameters(void) const ITK_OVERRIDE
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
  virtual void UpdateTransformParameters( const DerivativeType & update,
                                          ParametersValueType factor = 1.0 );

  /** Return the number of local parameters that completely defines the
   *  Transform at an individual voxel.
   *  For transforms with local support, this will enable downstream
   *  computation of the jacobian wrt only the local support region.
   *  For instance, in the case of a deformation field, this will be equal to
   *  the number of image dimensions. If it is an affine transform, this will
   *  be the same as the GetNumberOfParameters().
   */
  virtual NumberOfParametersType GetNumberOfLocalParameters(void) const
  {
    return this->GetNumberOfParameters();
  }

  /** Return the number of parameters that completely define the Transfom  */
  virtual NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return this->m_Parameters.Size();
  }

  /** Return the number of parameters that define the constant elements of a Transfom  */
  virtual NumberOfParametersType GetNumberOfFixedParameters() const
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
  bool GetInverse( Self *itkNotUsed(inverseTransform) ) const
  {
    return false;
  }

  /** Return an inverse of this transform. If the inverse has not been
   *  implemented, return ITK_NULLPTR. The type of the inverse transform
   *  does not necessarily need to match the type of the forward
   *  transform. This allows one to return a numeric inverse transform
   *  instead.
   */
  virtual InverseTransformBasePointer GetInverseTransform() const
  {
    return ITK_NULLPTR;
  }

  /** Generate a platform independent name */
  virtual std::string GetTransformTypeAsString() const ITK_OVERRIDE;

  typedef typename Superclass::TransformCategoryType    TransformCategoryType;

  /** Indicates the category transform.
   *  e.g. an affine transform, or a local one, e.g. a deformation field.
   */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Superclass::UnknownTransformCategory;
  }

  virtual bool IsLinear() const
  {
    return ( this->GetTransformCategory() == Superclass::Linear );
  }


#ifdef ITKV3_COMPATIBILITY
  /**
   * This function is only here for ITKv3 backwards compatibility.
   *
   * This is not a thread-safe version for GetJacobian(), because the internal
   * class member variable m_Jacobian could be changed for different values
   * in different threads.
   *
   * All derived classes should move the computations of computing a jacobian
   * from GetJacobian to ComputeJacobianWithRespectToParameters and then
   * use this forwarding function for backwards compatibility.
   */
  virtual const JacobianType & GetJacobian(const InputPointType  & x) const
  {
    this->ComputeJacobianWithRespectToParameters(x, m_SharedLocalJacobian);
    return m_SharedLocalJacobian;
  }

#endif

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
   *  To avoid repeatitive memory allocation, pass in 'jacobian' with its size
   *  already set. */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType  & itkNotUsed(p), JacobianType & itkNotUsed(jacobian) ) const = 0;

  virtual void ComputeJacobianWithRespectToParametersCachedTemporaries(const InputPointType  & p, JacobianType & jacobian, JacobianType & itkNotUsed(jacobianWithRespectToPosition) ) const
  {
    //NOTE: default implementation is not optimized, and just falls back to original methods.
    this->ComputeJacobianWithRespectToParameters(p, jacobian);
  }


  /** This provides the ability to get a local jacobian value
   *  in a dense/local transform, e.g. DisplacementFieldTransform. For such
   *  transforms it would be unclear what parameters would refer to.
   *  Generally, global transforms should return an indentity jacobian
   *  since there is no change with respect to position. */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType & itkNotUsed(x), JacobianType & itkNotUsed(jacobian) ) const
  {
    itkExceptionMacro(
      "ComputeJacobianWithRespectToPosition( InputPointType, JacobianType"
      " is unimplemented for " << this->GetNameOfClass() );
  }


  /** This provides the ability to get a local jacobian value
   *  in a dense/local transform, e.g. DisplacementFieldTransform. For such
   *  transforms it would be unclear what parameters would refer to.
   *  Generally, global transforms should return an indentity jacobian
   *  since there is no change with respect to position. */
  virtual void ComputeInverseJacobianWithRespectToPosition(const InputPointType & x, JacobianType & jacobian ) const;

protected:
  /**
   * Clone the current transform.
   * This does a complete copy of the transform
   * state to the new transform
   */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  Transform();
  Transform(NumberOfParametersType NumberOfParameters);
  virtual ~Transform() ITK_OVERRIDE { }

  mutable ParametersType      m_Parameters;
  mutable FixedParametersType m_FixedParameters;

  OutputDiffusionTensor3DType PreservationOfPrincipalDirectionDiffusionTensor3DReorientation(
    const InputDiffusionTensor3DType, const JacobianType ) const;

#ifdef ITKV3_COMPATIBILITY
  // This is only needed to provide the old interface that returns a reference to the Jacobian.
  // It is NOT thread-safe and should be avoided whenever possible.
  mutable JacobianType m_SharedLocalJacobian;
#endif

  mutable DirectionChangeMatrix m_DirectionChange;


private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Transform);

  template <typename TType>
  static std::string GetTransformTypeAsString(TType *)
  {
    std::string rval("other");

    return rval;
  }

  static std::string GetTransformTypeAsString(float *)
  {
    std::string rval("float");

    return rval;
  }

  static std::string GetTransformTypeAsString(double *)
  {
    std::string rval("double");

    return rval;
  }

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransform.hxx"
#endif

#endif
