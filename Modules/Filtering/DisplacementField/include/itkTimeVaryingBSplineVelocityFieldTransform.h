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
#ifndef itkTimeVaryingBSplineVelocityFieldTransform_h
#define itkTimeVaryingBSplineVelocityFieldTransform_h

#include "itkVelocityFieldTransform.h"

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
 * Note:  For readability of the code, it is important to note that we store the
 * control point lattice in the m_VelocityField variable since they are of the same
 * type.  It's only when we call IntegrateVelocityField() that a sampled velocity
 * field is created from the control point lattice.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup Transforms
 * \ingroup ITKDisplacementField
 */
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT TimeVaryingBSplineVelocityFieldTransform
  : public VelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TimeVaryingBSplineVelocityFieldTransform);

  /** Standard class type aliases. */
  using Self = TimeVaryingBSplineVelocityFieldTransform;
  using Superclass = VelocityFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TimeVaryingBSplineVelocityFieldTransform, VelocityFieldTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** InverseTransform type. */
  using InverseTransformBasePointer = typename Superclass::InverseTransformBasePointer;

  /** Interpolator types.*/
  using InterpolatorType = typename Superclass::InterpolatorType;
  using VelocityFieldIntegratorType = typename Superclass::VelocityFieldInterpolatorType;

  /** Field types. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using VelocityFieldType = typename Superclass::VelocityFieldType;

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Type of the input parameters. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename ParametersType::ValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename FixedParametersType::ValueType;
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** Derivative type */
  using DerivativeType = typename Superclass::DerivativeType;

  /** Dimension of the domain spaces. */
  static constexpr unsigned int Dimension = NDimensions;

  /** Dimension of the time varying velocity field. */
  static constexpr unsigned int VelocityFieldDimension = NDimensions + 1;

  using VelocityFieldPointType = typename VelocityFieldType::PointType;
  using VelocityFieldSizeType = typename VelocityFieldType::SizeType;
  using VelocityFieldSpacingType = typename VelocityFieldType::SpacingType;
  using VelocityFieldDirectionType = typename VelocityFieldType::DirectionType;

  using TimeVaryingVelocityFieldControlPointLatticeType = VelocityFieldType;
  using TimeVaryingVelocityFieldControlPointLatticePointer = typename VelocityFieldType::Pointer;

  using DisplacementVectorType = typename DisplacementFieldType::PixelType;
  using DisplacementFieldSizeType = typename DisplacementFieldType::SizeType;
  using DisplacementFieldSpacingType = typename DisplacementFieldType::SpacingType;
  using DisplacementFieldPointType = typename DisplacementFieldType::PointType;
  using DisplacementFieldDirectionType = typename DisplacementFieldType::DirectionType;

  /** Get the time-varying velocity field control point lattice. */
  VelocityFieldType *
  GetTimeVaryingVelocityFieldControlPointLattice()
  {
    return this->GetModifiableVelocityField();
  }

  /** Set the time-varying velocity field control point lattice.  */
  virtual void
  SetTimeVaryingVelocityFieldControlPointLattice(VelocityFieldType * fieldLattice)
  {
    this->SetVelocityField(fieldLattice);
  }

  /** Update the transform's parameters by the adding values in \c update
   * to current parameter values.  We assume \c update is of the same length as Parameters.
   * Throw exception otherwise. \c factor is a scalar multiplier for each value in update.
   * SetParameters is called at the end of this method, to allow transforms
   * to perform any required operations on the update parameters, typically
   * a conversion to member variables for use in TransformPoint.
   */
  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Trigger the computation of the displacement field by integrating the time-varying velocity field. */
  void
  IntegrateVelocityField() override;

  /** Set/Get sampled velocity field origin */
  itkSetMacro(VelocityFieldOrigin, VelocityFieldPointType);
  itkGetConstMacro(VelocityFieldOrigin, VelocityFieldPointType);

  /** Set/Get sampled velocity field spacing */
  itkSetMacro(VelocityFieldSpacing, VelocityFieldSpacingType);
  itkGetConstMacro(VelocityFieldSpacing, VelocityFieldSpacingType);

  /** Set/Get sampled velocity field size */
  itkSetMacro(VelocityFieldSize, VelocityFieldSizeType);
  itkGetConstMacro(VelocityFieldSize, VelocityFieldSizeType);

  /** Set/Get sampled velocity field direction */
  itkSetMacro(VelocityFieldDirection, VelocityFieldDirectionType);
  itkGetConstMacro(VelocityFieldDirection, VelocityFieldDirectionType);

  /** Set/Get the spline order. */
  itkSetMacro(SplineOrder, unsigned int);
  itkGetConstMacro(SplineOrder, unsigned int);

protected:
  TimeVaryingBSplineVelocityFieldTransform();
  ~TimeVaryingBSplineVelocityFieldTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_SplineOrder;
  bool         m_TemporalPeriodicity;

  VelocityFieldPointType     m_VelocityFieldOrigin;
  VelocityFieldSpacingType   m_VelocityFieldSpacing;
  VelocityFieldDirectionType m_VelocityFieldDirection;
  VelocityFieldSizeType      m_VelocityFieldSize;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTimeVaryingBSplineVelocityFieldTransform.hxx"
#endif

#endif // itkTimeVaryingBSplineVelocityFieldTransform_h
