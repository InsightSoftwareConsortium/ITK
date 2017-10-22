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
#ifndef itkTimeVaryingVelocityFieldTransform_h
#define itkTimeVaryingVelocityFieldTransform_h

#include "itkVelocityFieldTransform.h"

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
template<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT TimeVaryingVelocityFieldTransform :
  public VelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef TimeVaryingVelocityFieldTransform                         Self;
  typedef VelocityFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( TimeVaryingVelocityFieldTransform, VelocityFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer InverseTransformBasePointer;

  /** Interpolator types.*/
  typedef typename Superclass::InterpolatorType                     InterpolatorType;
  typedef typename Superclass::VelocityFieldInterpolatorType        VelocityFieldIntegratorType;

  /** Field types. */
  typedef typename Superclass::DisplacementFieldType                DisplacementFieldType;
  typedef typename Superclass::VelocityFieldType                    VelocityFieldType;

  typedef VelocityFieldType                                         TimeVaryingVelocityFieldType;
  typedef typename VelocityFieldType::Pointer                       TimeVaryingVelocityFieldPointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType              ScalarType;

  /** Type of the input parameters. */
  typedef typename Superclass::ParametersType          ParametersType;
  typedef typename ParametersType::ValueType           ParametersValueType;
  typedef typename Superclass::FixedParametersType     FixedParametersType;
  typedef typename FixedParametersType::ValueType      FixedParametersValueType;
  typedef typename Superclass::NumberOfParametersType  NumberOfParametersType;

  /** Derivative type */
  typedef typename Superclass::DerivativeType          DerivativeType;

  typedef typename Transform<TParametersValueType,NDimensions, NDimensions>::Pointer TransformPointer;

  /** Get the time-varying velocity field. */
#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  VelocityFieldType * GetTimeVaryingVelocityField()
    {
    return this->GetModifiableVelocityField();
    }
#endif
  VelocityFieldType * GetModifiableTimeVaryingVelocityField()
    {
    return this->GetModifiableVelocityField();
    }
  const VelocityFieldType * GetTimeVaryingVelocityField() const
    {
    return this->GetVelocityField();
    }

  /** Set the time-varying velocity field.  */
  virtual void SetTimeVaryingVelocityField( VelocityFieldType * field )
    {
    this->SetVelocityField( field );
    }

  /** Trigger the computation of the displacement field by integrating
   * the time-varying velocity field. */
  virtual void IntegrateVelocityField() ITK_OVERRIDE;

protected:
  TimeVaryingVelocityFieldTransform();
  virtual ~TimeVaryingVelocityFieldTransform() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TimeVaryingVelocityFieldTransform);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkTimeVaryingVelocityFieldTransform.hxx"
#endif

#endif // itkTimeVaryingVelocityFieldTransform_h
