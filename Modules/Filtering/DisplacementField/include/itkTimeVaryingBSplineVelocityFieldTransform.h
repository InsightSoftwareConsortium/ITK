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
#ifndef __itkTimeVaryingBSplineVelocityFieldTransform_h
#define __itkTimeVaryingBSplineVelocityFieldTransform_h

#include "itkTimeVaryingVelocityFieldTransform.h"

namespace itk
{

/**
 * \class TimeVaryingBSplineVelocityFieldTransform
 * \brief Integrate a time-varying velocity field represented by a B-spline control
 * point lattice.
 *
 * Diffeomorphisms are topology-preserving mappings that are useful for
 * describing biologically plausible deformations.  Mathematically, a
 * diffeomorphism, \f$ \phi \f$, is generated from a time-varying velocity field, v, as
 * described by the integral equation:
 *
 * \f[
 * \phi(t_b) = \phi(t_a) + \int_{t_a}^{t_b} v(\phi(t),t) dt
 * \f]
 *
 * In typical registration
 * applications it is computationally more efficient to sample the B-spline transform
 * to its corresponding displacement field.  Therefore, the user needs to specify the
 * domain parameters of that displacement field using the following functions:
 *
 *   \li \c SetDisplacementFieldSpacing()
 *   \li \c SetDisplacementFieldOrigin()
 *   \li \c SetDisplacementFieldSize()
 *   \li \c SetDisplacementFieldDirection()
 *
 * It's important that these parameters match up with the fixed parameters of this
 * transform which are defined as the parameters of the (N+1)-D B-spline grid
 * representing the continuous velocity field.  This control point lattice is set
 * using \c SetTimeVaryingVelocityFieldControlPointLattice() or it can be created
 * by setting the fixed parameters.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup Transforms
 * \ingroup ITKDisplacementField
 */
template<class TScalar, unsigned int NDimensions>
class ITK_EXPORT TimeVaryingBSplineVelocityFieldTransform :
  public TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef TimeVaryingBSplineVelocityFieldTransform                 Self;
  typedef TimeVaryingVelocityFieldTransform<TScalar, NDimensions>  Superclass;
  typedef SmartPointer<Self>                                       Pointer;
  typedef SmartPointer<const Self>                                 ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingBSplineVelocityFieldTransform, TimeVaryingVelocityFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkSimpleNewMacro( Self );

  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer InverseTransformBasePointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType              ScalarType;

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

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Dimension of the time varying velocity field. */
  itkStaticConstMacro( TimeVaryingVelocityFieldDimension, unsigned int, NDimensions+1 );

  /**
   * Define the time-varying velocity field type.
   */
  typedef Image<OutputVectorType, TimeVaryingVelocityFieldDimension>          TimeVaryingVelocityFieldControlPointLatticeType;
  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::Pointer   TimeVaryingVelocityFieldControlPointLatticePointer;
  typedef TimeVaryingVelocityFieldControlPointLatticeType                     TimeVaryingVelocityFieldType;

  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::PointType         VelocityFieldPointType;
  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType          VelocityFieldSizeType;
  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType       VelocityFieldSpacingType;
  typedef typename TimeVaryingVelocityFieldControlPointLatticeType::DirectionType     VelocityFieldDirectionType;

  typedef typename Superclass::DisplacementFieldType                       DisplacementFieldControlPointLatticeType;
  typedef typename Superclass::DisplacementFieldType                       DisplacementFieldType;
  typedef typename DisplacementFieldType::PixelType                        DisplacementVectorType;
  typedef typename DisplacementFieldType::SizeType                         DisplacementFieldSizeType;
  typedef typename DisplacementFieldType::SpacingType                      DisplacementFieldSpacingType;
  typedef typename DisplacementFieldType::PointType                        DisplacementFieldPointType;
  typedef typename DisplacementFieldType::DirectionType                    DisplacementFieldDirectionType;

  /** Get the time-varying velocity field control point lattice. */
  itkGetObjectMacro( TimeVaryingVelocityFieldControlPointLattice, TimeVaryingVelocityFieldControlPointLatticeType );

  /** Set the time-varying velocity field control point lattice.  */
  virtual void SetTimeVaryingVelocityFieldControlPointLattice( TimeVaryingVelocityFieldControlPointLatticeType * );

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters( const ParametersType & );

  /** Update the transform's parameters by the adding values in \c update
   * to current parameter values.  We assume \c update is of the same length as Parameters.
   * Throw exception otherwise. \c factor is a scalar multiplier for each value in update.
   * SetParameters is called at the end of this method, to allow transforms
   * to perform any required operations on the update parameters, typically
   * a converion to member variables for use in TransformPoint.
   */
  virtual void UpdateTransformParameters( DerivativeType & update, ScalarType factor = 1.0 );

  /** Trigger the computation of the displacement field by integrating the time-varying velocity field. */
  virtual void IntegrateVelocityField();

  /** Return an inverse of this transform. */
  bool GetInverse( Self *inverse ) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const;

  /** Set/Get spline order. */
  itkSetMacro( SplineOrder, unsigned int );
  itkGetConstMacro( SplineOrder, unsigned int );

  /** Set/Get temporal periodicity. */
  itkSetMacro( TemporalPeriodicity, bool );
  itkGetConstMacro( TemporalPeriodicity, bool );
  itkBooleanMacro( TemporalPeriodicity );

  /** Set/Get sampled velocity field origin */
  itkSetMacro( VelocityFieldOrigin, VelocityFieldPointType );
  itkGetConstMacro( VelocityFieldOrigin, VelocityFieldPointType );

  /** Set/Get sampled velocity field spacing */
  itkSetMacro( VelocityFieldSpacing, VelocityFieldSpacingType );
  itkGetConstMacro( VelocityFieldSpacing, VelocityFieldSpacingType );

  /** Set/Get sampled velocity field size */
  itkSetMacro( VelocityFieldSize, VelocityFieldSizeType );
  itkGetConstMacro( VelocityFieldSize, VelocityFieldSizeType );

  /** Set/Get sampled velocity field direction */
  itkSetMacro( VelocityFieldDirection, VelocityFieldDirectionType );
  itkGetConstMacro( VelocityFieldDirection, VelocityFieldDirectionType );

protected:
  TimeVaryingBSplineVelocityFieldTransform();
  virtual ~TimeVaryingBSplineVelocityFieldTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

private:
  TimeVaryingBSplineVelocityFieldTransform( const Self& ); //purposely not implementen
  void operator=( const Self& ); //purposely not implemented

  /** The deformation field and its inverse (if it exists). */
  TimeVaryingVelocityFieldControlPointLatticePointer             m_TimeVaryingVelocityFieldControlPointLattice;

  unsigned int                                                   m_SplineOrder;
  bool                                                           m_TemporalPeriodicity;

  VelocityFieldPointType                                         m_VelocityFieldOrigin;
  VelocityFieldSpacingType                                       m_VelocityFieldSpacing;
  VelocityFieldDirectionType                                     m_VelocityFieldDirection;
  VelocityFieldSizeType                                          m_VelocityFieldSize;
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkTimeVaryingBSplineVelocityFieldTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkTimeVaryingBSplineVelocityFieldTransform.hxx"
#endif

#endif // __itkTimeVaryingBSplineVelocityFieldTransform_h
