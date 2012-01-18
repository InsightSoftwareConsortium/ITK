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
#ifndef __itkTimeVaryingVelocityFieldTransform_h
#define __itkTimeVaryingVelocityFieldTransform_h

#include "itkDisplacementFieldTransform.h"

#include "itkImageVectorOptimizerParametersHelper.h"

namespace itk
{

/** \class TimeVaryingVelocityFieldTransform
 * \brief Transform objects based on integration of a time-varying velocity
 * field.
 *
 * Diffeomorphisms are topology-preserving mappings that are useful for
 * describing biologically plausible deformations.  Mathematically, a
 * diffeomorphism, \f$\phi\f$, is generated from a time-varying velocity field, v, as
 * described by the first-order differential equation:
 *
 *  \f[
 *    v(\phi(x,t), t) = \frac{d\phi(x, t)}{dt}, \phi(x, 0) = x
 *  \f]
 *
 * In this class, the input is the time-varying velocity field. The output
 * diffeomorphism is produced using fourth order Runge-Kutta.
 *
 * \warning The output deformation field needs to have dimensionality of 1
 * less than the input time-varying velocity field. It is assumed that the
 * last dimension of the time-varying velocity field corresponds to Time,
 * and the other dimensions represent Space.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup Transforms
 * \ingroup ITKDisplacementField
 */
template<class TScalar, unsigned int NDimensions>
class ITK_EXPORT TimeVaryingVelocityFieldTransform :
  public DisplacementFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef TimeVaryingVelocityFieldTransform                 Self;
  typedef DisplacementFieldTransform<TScalar, NDimensions>  Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingVelocityFieldTransform, DisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** implement type-specific clone method */
  itkTransformCloneMacro();

  /** InverseTransform type. */
  typedef typename Superclass:: InverseTransformBasePointer InverseTransformBasePointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType          ScalarType;

  /** Type of the input parameters. */
  typedef typename Superclass::ParametersType          ParametersType;
  typedef typename ParametersType::ValueType           ParametersValueType;
  typedef typename Superclass::NumberOfParametersType  NumberOfParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType        JacobianType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType      InputPointType;
  typedef typename Superclass::OutputPointType     OutputPointType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType     InputVectorType;
  typedef typename Superclass::OutputVectorType    OutputVectorType;

  /** Derivative type */
  typedef typename Superclass::DerivativeType       DerivativeType;

  typedef typename Transform<TScalar,NDimensions,NDimensions>::Pointer TransformPointer;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Dimension of the time varying velocity field. */
  itkStaticConstMacro( TimeVaryingVelocityFieldDimension, unsigned int, NDimensions+1 );

  /**
   * Define the time-varying velocity field type and corresponding interpolator
   * type.
   */
  typedef Image<OutputVectorType, TimeVaryingVelocityFieldDimension>  TimeVaryingVelocityFieldType;
  typedef typename TimeVaryingVelocityFieldType::Pointer              TimeVaryingVelocityFieldPointer;
  typedef typename TimeVaryingVelocityFieldType::PointType            TimeVaryingVelocityFieldPointType;
  typedef typename TimeVaryingVelocityFieldType::SpacingType          TimeVaryingVelocityFieldSpacingType;
  typedef typename TimeVaryingVelocityFieldType::DirectionType        TimeVaryingVelocityFieldDirectionType;

  typedef VectorInterpolateImageFunction<TimeVaryingVelocityFieldType, ScalarType>  TimeVaryingVelocityFieldInterpolatorType;
  typedef typename TimeVaryingVelocityFieldInterpolatorType::Pointer                TimeVaryingVelocityFieldInterpolatorPointer;

  typedef typename TimeVaryingVelocityFieldType::SizeType          SizeType;
  typedef typename TimeVaryingVelocityFieldType::PointType         PointType;
  typedef typename TimeVaryingVelocityFieldType::SpacingType       SpacingType;
  typedef typename TimeVaryingVelocityFieldType::DirectionType     DirectionType;

  typedef typename Superclass::DisplacementFieldType               DisplacementFieldType;
  typedef typename DisplacementFieldType::PixelType                DisplacementVectorType;

  /** Define the internal parameter helper used to access the field */
  typedef ImageVectorOptimizerParametersHelper
    <ScalarType, OutputVectorType::Dimension,
    itkGetStaticConstMacro( Dimension ) + 1>      OptimizerParametersHelperType;

  /** Get the time-varying deformation field. */
  itkGetObjectMacro( TimeVaryingVelocityField, TimeVaryingVelocityFieldType );

  /** Set the time-varying field.  */
  virtual void SetTimeVaryingVelocityField( TimeVaryingVelocityFieldType * );

  /** Set the interpolator for the time-varying velocity field. */
  itkSetObjectMacro( TimeVaryingVelocityFieldInterpolator,
    TimeVaryingVelocityFieldInterpolatorType );

  /** Get the interpolator for the time-varying velocity field. */
  itkGetConstObjectMacro( TimeVaryingVelocityFieldInterpolator,
    TimeVaryingVelocityFieldInterpolatorType );

  /**
   * Set the deformation field. We want to override the base class
   * implementation since we don't want to optimize over the deformation
   * field for this class but rather the time-varying velocity field
   */
  itkSetObjectMacro( DisplacementField, DisplacementFieldType );

  /**
   * Set the transformation parameters. This sets the time-varying velocity
   * field image directly.
   */
  virtual void SetParameters( const ParametersType & );

  /** Trigger the computation of the displacement field by integrating
   * the time-varying velocity field. */
  virtual void IntegrateVelocityField();

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters( const ParametersType & );

  /** Get the Fixed Parameters. */

  virtual void UpdateTransformParameters( DerivativeType &,
    ScalarType factor = 1.0 );

  /** Return an inverse of this transform. */
  bool GetInverse( Self *inverse ) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const;

  /** This transform is not linear. */
  virtual bool IsLinear() const { return false; }

  /** Get the number of local parameters */
  NumberOfParametersType GetNumberOfLocalParameters() const
    {
    return Dimension;
    }

  /** Does the transform have local support */
  virtual bool HasLocalSupport() const
    {
    return true;
    }

  /**
   * Set the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro( LowerTimeBound, ScalarType, 0, 1 );

  /**
   * Get the lower time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro( LowerTimeBound, ScalarType );

  /**
   * Set the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkSetClampMacro( UpperTimeBound, ScalarType, 0, 1 );

  /**
   * Get the upper time bound defining the integration domain of the transform.
   * We assume that the total possible time domain is [0,1]
   */
  itkGetConstMacro( UpperTimeBound, ScalarType );

  /**
   * Set the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 100;
   */
  itkSetMacro( NumberOfIntegrationSteps, unsigned int );

  /**
   * Get the number of integration steps used in the Runge-Kutta solution of the
   * initial value problem.  Default = 100;
   */
  itkGetConstMacro( NumberOfIntegrationSteps, unsigned int );

protected:
  TimeVaryingVelocityFieldTransform();
  virtual ~TimeVaryingVelocityFieldTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  ScalarType                                m_LowerTimeBound;
  ScalarType                                m_UpperTimeBound;

  /** Clone the current transform */
  virtual TransformPointer InternalClone() const;

  typename DisplacementFieldType::Pointer
    CopyDisplacementField(const DisplacementFieldType *toCopy) const;
  /**
   * Convenience method which reads the information from the current
   * displacement field into m_FixedParameters.
   */
  virtual void SetFixedParametersFromTimeVaryingVelocityField();

  /** The deformation field and its inverse (if it exists). */
  typename TimeVaryingVelocityFieldType::Pointer    m_TimeVaryingVelocityField;
  TimeVaryingVelocityFieldInterpolatorPointer       m_TimeVaryingVelocityFieldInterpolator;
  unsigned int                                      m_NumberOfIntegrationSteps;

private:
  TimeVaryingVelocityFieldTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkTimeVaryingVelocityFieldTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkTimeVaryingVelocityFieldTransform.hxx"
#endif

#endif // __itkTimeVaryingVelocityFieldTransform_h
