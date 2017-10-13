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
#ifndef itkDisplacementFieldTransform_h
#define itkDisplacementFieldTransform_h

#include "itkTransform.h"

#include "itkImage.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkImageVectorOptimizerParametersHelper.h"
#include "itkVectorInterpolateImageFunction.h"

namespace itk
{

/** \class DisplacementFieldTransform
 * \brief Provides local/dense/high-dimensionaltiy transformation via a
 * a displacement field.
 *
 * The displacement field stores vectors of displacements, with
 * dimension \c NDimensions. Transformation is performed at a given
 * point by adding the displacement at that point to the input point.
 *
 * T(x, p), x is the position, p is the local parameter at position x.
 * For a 2D example:
 *
 *  x = (x0, x1), p = (p0, p1)
 *
 * then T(x, p) is defined as:
 *
 *    T(x, p) = (T0, T1) = (x0+p0, x1+p1)
 *
 * During transformation, out-of-bounds input points are returned
 * with zero displacement.
 *
 * The displacement field is defined using an itkImage, and must be set
 * before use by the user, using \c SetDisplacementField. The image has
 * the same dimensionality as the input and output spaces, defined by
 * template parameter \c NDimensions, and is an image of vectors of
 * type \c OutputVectorType, with dimensionality NDimensions as well.
 *
 * An interpolator of type \c VectorInterpolateImageFunction is used with
 * the displacement field image. By default,
 * VectorLinearInterpolateImageFunction is used, and the user can override
 * using SetInterpolator.
 *
 * The displacement field data is stored using the common
 * \c OptimizerParameters type
 * in conjunction with the \c ImageVectorOptimizerParametersHelper class. This
 * allows access of the displacement field image as if it were an itkArray,
 * allowing transparent use with other classes.
 * \warning The \c SetParameters
 * method will copy the passed parameters, which can be costly since
 * displacement fields are dense and thus potentially very large.
 *
 * The \c UpdateTransformParameters method simply adds the provided
 * update array, applying the usual optional scaling factor. Derived
 * classes may provide different behavior.
 *
 * Because this is a local transform, methods that have a version that takes
 * a point must be used, such as \c TransformVector,
 * \c TransformCovariantVector, and \c TransformDiffusionTensor. Also,
 * \c ComputeJacobianWithRespectToParameters simply returns
 * an identity matrix (see method documentation),
 * and \c ComputeJacobianWithRespectToPosition should be used.
 *
 *
 * \ingroup ITKDisplacementField
 */
template
<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT DisplacementFieldTransform :
  public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef DisplacementFieldTransform                                Self;
  typedef Transform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( DisplacementFieldTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer InverseTransformBasePointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Type of the input parameters. */
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  typedef typename Superclass::InputVectorPixelType  InputVectorPixelType;
  typedef typename Superclass::OutputVectorPixelType OutputVectorPixelType;

  /** Standard covariant vector type for this class */
  typedef typename Superclass::InputCovariantVectorType
  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
  OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

  /** Standard diffusion tensor type for this class */
  typedef typename Superclass::InputDiffusionTensor3DType
  InputDiffusionTensor3DType;
  typedef typename Superclass::OutputDiffusionTensor3DType
  OutputDiffusionTensor3DType;

  /** Standard tensor type for this class */
  typedef CovariantVector<ScalarType, InputDiffusionTensor3DType::Dimension>
  InputTensorEigenVectorType;
  typedef CovariantVector<ScalarType, OutputDiffusionTensor3DType::Dimension>
  OutputTensorEigenVectorType;
  /** Derivative type */
  typedef typename Superclass::DerivativeType DerivativeType;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Define the displacement field type and corresponding interpolator type. */
  typedef Image<OutputVectorType,  Dimension>          DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer      DisplacementFieldPointer;
  typedef typename DisplacementFieldType::ConstPointer DisplacementFieldConstPointer;

  typedef VectorInterpolateImageFunction
    <DisplacementFieldType, ScalarType> InterpolatorType;

  /** Standard types for the displacement Field */
  typedef typename DisplacementFieldType::IndexType      IndexType;
  typedef typename DisplacementFieldType::RegionType     RegionType;
  typedef typename DisplacementFieldType::SizeType       SizeType;
  typedef typename DisplacementFieldType::SpacingType    SpacingType;
  typedef typename DisplacementFieldType::DirectionType  DirectionType;
  typedef typename DisplacementFieldType::PointType      PointType;
  typedef typename DisplacementFieldType::PixelType      PixelType;

  /** Define the internal parameter helper used to access the field */
  typedef ImageVectorOptimizerParametersHelper<
    ScalarType,
    OutputVectorType::Dimension,
    Dimension>
  OptimizerParametersHelperType;

  /** Get/Set the displacement field.
   * Set the displacement field. Create special set accessor to update
   * interpolator and assign displacement field to transform parameters
   * container. */
  virtual void SetDisplacementField( DisplacementFieldType* field );
  itkGetModifiableObjectMacro(DisplacementField, DisplacementFieldType );

  /** Get/Set the inverse displacement field. This must be supplied by the user for
   * GetInverse() to work. */
  virtual void SetInverseDisplacementField( DisplacementFieldType * inverseDisplacementField );
  itkGetModifiableObjectMacro(InverseDisplacementField, DisplacementFieldType );

  /** Get/Set the interpolator.
   * Create out own set accessor that assigns the displacement field. */
  virtual void SetInterpolator( InterpolatorType* interpolator );
  itkGetModifiableObjectMacro( Interpolator, InterpolatorType );

  /** Get/Set the interpolator for the inverse field.
   * Create out own set accessor that assigns the displacement field. */
  virtual void SetInverseInterpolator( InterpolatorType* interpolator );
  itkGetModifiableObjectMacro(InverseInterpolator, InterpolatorType );

  /** Get the modification time of displacement field. */
  itkGetConstReferenceMacro( DisplacementFieldSetTime, ModifiedTimeType );

  /**  Method to transform a point. Out-of-bounds points will
   * be returned with zero displacemnt. */
  virtual OutputPointType TransformPoint( const InputPointType& thisPoint ) const ITK_OVERRIDE;

  /**  Method to transform a vector. */
  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "TransformVector(Vector) unimplemented, use "
                       "TransformVector(Vector,Point)" );
  }

  virtual OutputVectorPixelType TransformVector(const InputVectorPixelType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "TransformVector(Vector) unimplemented, use "
                       "TransformVector(Vector,Point)" );
  }

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "TransformVector(Vector) unimplemented, use "
                       "TransformVector(Vector,Point)" );
  }

  /** Method to transform a tensor. */
  using Superclass::TransformDiffusionTensor3D;
  OutputDiffusionTensor3DType TransformDiffusionTensor(
    const InputDiffusionTensor3DType & ) const
  {
    itkExceptionMacro( "TransformDiffusionTensor(Tensor) unimplemented, use "
                       "TransformDiffusionTensor(Tensor,Point)" );
  }

  OutputVectorPixelType TransformDiffusionTensor(const InputVectorPixelType & )
  const
  {
    itkExceptionMacro( "TransformDiffusionTensor(Tensor) unimplemented, use "
                       "TransformDiffusionTensor(Tensor,Point)" );
  }

  /** Method to transform a CovariantVector. */
  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "TransformCovariantVector(CovariantVector) "
                       "unimplemented, use TransformCovariantVector(CovariantVector,Point)" );
  }

  virtual OutputVectorPixelType TransformCovariantVector(
    const InputVectorPixelType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "TransformCovariantVector(CovariantVector) "
                       "unimplemented, use TransformCovariantVector(CovariantVector,Point)" );
  }

  /** Set the transformation parameters. This sets the displacement
   * field image directly. */
  virtual void SetParameters(const ParametersType & params) ITK_OVERRIDE
  {
    if( &(this->m_Parameters) != &params )
      {
      if( params.Size() != this->m_Parameters.Size() )
        {
        itkExceptionMacro("Input parameters size (" << params.Size()
                                                    << ") does not match internal size ("
                                                    << this->m_Parameters.Size() << ").");
        }
      // Copy into existing object
      this->m_Parameters = params;
      this->Modified();
      }
  }

  /**
   * This method sets the fixed parameters of the transform.
   * For a displacement field transform, the fixed parameters are the
   * following: field size, field origin, field spacing, and field direction.
   *
   * Note: If a displacement field already exists, this function
   * creates a new one with zero displacement (identity transform). If
   * an inverse displacement field exists, a new one is also created.
   */
  virtual void SetFixedParameters( const FixedParametersType & ) ITK_OVERRIDE;

  /**
   * Compute the jacobian with respect to the parameters at a point.
   * Simply returns identity matrix, sized [NDimensions, NDimensions].
   *
   * T(x, p), x is the position, p is the local parameter at position x.
   * Take a 2D example, x = (x0, x1), p = (p0, p1) and T(x, p) is defined as:
   *
   *    T(x, p) = (T0, T1) = (x0+p0, x1+p1)
   *
   * Each local deformation is defined as a translation transform.
   * So the Jacobian w.r.t parameters are
   *
   * dT/dp =
   *    [ dT0/dp0, dT0/dp1;
   *      dT1/dp0, dT1/dp1 ];
   *
   *    = [1, 0;
   *       0, 1];
   *
   * TODO: format the above for doxygen formula.
   */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType &,
                                                      JacobianType & j) const ITK_OVERRIDE
  {
    j = this->m_IdentityJacobian;
  }

  /**
   * Compute the jacobian with respect to the parameters at an index.
   * Simply returns identity matrix, sized [NDimensions, NDimensions].
   * See \c ComputeJacobianWithRespectToParameters( InputPointType, ... )
   * for rationale.
   */
  virtual void ComputeJacobianWithRespectToParameters(const IndexType &,
                                                      JacobianType & j) const
  {
    j = this->m_IdentityJacobian;
  }

  /**
   * Compute the jacobian with respect to the position, by point.
   * \c j will be resized as needed.
   */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType  & x, JacobianType & j ) const ITK_OVERRIDE;

  /**
   * Compute the jacobian with respect to the position, by point.
   * \c j will be resized as needed.
   */
  virtual void ComputeInverseJacobianWithRespectToPosition(const InputPointType  & x, JacobianType & j ) const ITK_OVERRIDE;

  /**
   * Compute the jacobian with respect to the position, by index.
   * \c j will be resized as needed.
   */
  virtual void ComputeJacobianWithRespectToPosition(const IndexType  & x, JacobianType & j ) const;

  /**
   * Compute the inverse jacobian of the forward displacement field with
   * respect to the position, by point. Note that this is different than
   * the jacobian of the inverse displacement field. This takes advantage
   * of the ability to compute the inverse jacobian of a displacement field
   * by simply reversing the sign of the forward jacobian.
   * However, a more accurate method for computing the inverse
   * jacobian is to take the inverse of the jacobian matrix. This
   * method is more computationally expensive and may be used by
   * setting \c useSVD to true
   */
  virtual void GetInverseJacobianOfForwardFieldWithRespectToPosition(const InputPointType & point,
                                                                     JacobianType & jacobian,
                                                                     bool useSVD = false ) const;

  /**
   * Compute the inverse jacobian of the forward displacement field with
   * respect to the position, by index.Note that this is different than
   * the jacobian of the inverse displacement field. This takes advantage
   * of the ability to compute the inverse jacobian of a displacement field
   * by simply reversing the sign of the forward jacobian.
   * However, a more accurate method for computing the inverse
   * jacobian is to take the inverse of the jacobian matrix. This
   * method is more computationally expensive and may be used by
   * setting \c useSVD to true
   */
  virtual void GetInverseJacobianOfForwardFieldWithRespectToPosition(const IndexType & index, JacobianType & jacobian,
                                                                     bool useSVD = false ) const;

  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /** Return an inverse of this transform.
   * Note that the inverse displacement field must be set by the user. */
  bool GetInverse( Self *inverse ) const;

  /** Return an inverse of this transform.
   * Note that the inverse displacement field must be set by the user. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  virtual void SetIdentity();

  /** This transform is not linear. */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Self::DisplacementField;
  }

  virtual NumberOfParametersType GetNumberOfLocalParameters(void) const ITK_OVERRIDE
  {
    return Dimension;
  }

  /** Set/Get the coordinate tolerance.
   *  This tolerance is used when comparing the space defined
   *  by deformation fields and it's inverse to ensure they occupy the
   *  same physical space.
   *
   * \sa ImageToImageFilterCommon::SetGlobalDefaultCoordinateTolerance
   */
  itkSetMacro(CoordinateTolerance, double);
  itkGetConstMacro(CoordinateTolerance, double);

  /** Set/Get the direction tolerance.
   *  This tolerance is used to when comparing the orientation of the
   *  deformation fields and it's inverse to ensure they occupy the
   *  same physical space.
   *
   * \sa ImageToImageFilterCommon::SetGlobalDefaultDirectionTolerance
   */
  itkSetMacro(DirectionTolerance, double);
  itkGetConstMacro(DirectionTolerance, double);

protected:

  DisplacementFieldTransform();
  virtual ~DisplacementFieldTransform() ITK_OVERRIDE;
  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** The displacement field and its inverse (if it exists). */
  typename DisplacementFieldType::Pointer      m_DisplacementField;
  typename DisplacementFieldType::Pointer      m_InverseDisplacementField;

  /** The interpolator. */
  typename InterpolatorType::Pointer          m_Interpolator;
  typename InterpolatorType::Pointer          m_InverseInterpolator;

  /** Track when the displacement field was last set/assigned, as
   * distinct from when it may have had its contents modified. */
  ModifiedTimeType m_DisplacementFieldSetTime;

  /** Create an identity jacobian for use in
   * ComputeJacobianWithRespectToParameters. */
  JacobianType m_IdentityJacobian;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DisplacementFieldTransform);

  /** Internal method for calculating either forward or inverse jacobian,
   * depending on state of \c doInverseJacobian. Used by
   * public methods \c ComputeJacobianWithRespectToPosition and
   * \c GetInverseJacobianOfForwardFieldWithRespectToPosition to
   * perform actual work.
   * \c doInverseJacobian indicates that the inverse jacobian
   * should be returned
   */
  virtual void ComputeJacobianWithRespectToPositionInternal(const IndexType & index, JacobianType & jacobian,
                                                            bool doInverseJacobian) const;

  /**
   * Internal method to check that the inverse and forward displacement fields have the
   * same fixed parameters.
   */
  virtual void VerifyFixedParametersInformation();

  /**
   * Convenience method which reads the information from the current
   * displacement field into m_FixedParameters.
   */
  virtual void SetFixedParametersFromDisplacementField() const;

  double m_CoordinateTolerance;
  double m_DirectionTolerance;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDisplacementFieldTransform.hxx"
#endif

#endif // itkDisplacementFieldTransform_h
