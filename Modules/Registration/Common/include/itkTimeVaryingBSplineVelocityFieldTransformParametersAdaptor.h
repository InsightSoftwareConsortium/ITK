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
#ifndef itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor_h
#define itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor_h

#include "itkTransformParametersAdaptor.h"

namespace itk
{
/** \class TimeVaryingBSplineVelocityFieldTransformParametersAdaptor
 * \brief TimeVaryingBSplineVelocityFieldTransformParametersAdaptor is a helper class intended to
 * definition.
 *
 * The fixed parameters store the following information:
 * \li B-spline mesh size
 * \li field origin
 * \li domain spacing
 * \li domain size  (note that domain_spacing * (domain_size - 1) = physical dimensions of transform domain)
 * \li field direction
 * During multiresolution image registration it is often desired to also increase
 * the displacement field resolution for greater flexibility in optimizing the
 * transform.  As defined in the base class, the user can change the resolution via
 *
     \code
     transformAdaptor->SetTransform( transform );
     transformAdaptor->SetRequiredFixedParameters( fixedParameters );
     transformAdaptor->AdaptTransformParameters();
     \endcode
 *
 * or the user can use the more intuitive API for setting the fixed parameters.
 * E.g., often the user will want to maintain the same transform domain spatial
 * extent but only increase the field size and decrease the spacing.  This can
 * be done as follows:
 *
     \code
     transformAdaptor->SetRequiredTransformDomainOrigin( displacementField->GetOrigin() );
     transformAdaptor->SetRequiredTransformDomainDirection( displacementField->GetDirection() );
     transformAdaptor->SetRequiredTransformDomainSize( requiredSize );
     transformAdaptor->SetRequiredTransformDomainSpacing( requiredSpacing );
     transformAdaptor->SetRequiredTransformDomainMeshSize( requiredMeshSize );
     transformAdaptor->SetSplineOrder( transform->GetSplineOrder() );
     transformAdaptor->AdaptTransformParameters();
     \endcode
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class ITK_TEMPLATE_EXPORT TimeVaryingBSplineVelocityFieldTransformParametersAdaptor
  : public TransformParametersAdaptor<TTransform>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TimeVaryingBSplineVelocityFieldTransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = TimeVaryingBSplineVelocityFieldTransformParametersAdaptor;
  using Superclass = TransformParametersAdaptor<TTransform>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TimeVaryingBSplineVelocityFieldTransformParametersAdaptor, TransformParametersAdaptor);

  /** Typedefs associated with the transform */
  using TransformType = TTransform;
  using TransformPointer = typename TransformType::Pointer;
  using ParametersType = typename TransformType::ParametersType;
  using ParametersValueType = typename TransformType::ParametersValueType;
  using FixedParametersType = typename TransformType::FixedParametersType;
  using FixedParametersValueType = typename TransformType::FixedParametersValueType;

  using TimeVaryingVelocityFieldControlPointLatticeType =
    typename TransformType::TimeVaryingVelocityFieldControlPointLatticeType;
  using TimeVaryingVelocityFieldControlPointLatticePointer =
    typename TimeVaryingVelocityFieldControlPointLatticeType::Pointer;
  using RegionType = typename TimeVaryingVelocityFieldControlPointLatticeType::RegionType;
  using IndexType = typename TimeVaryingVelocityFieldControlPointLatticeType::IndexType;
  using VectorType = typename TimeVaryingVelocityFieldControlPointLatticeType::PixelType;
  using OriginType = typename TimeVaryingVelocityFieldControlPointLatticeType::PointType;
  using SpacingType = typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType;
  using SizeType = typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType;
  using SizeValueType = typename TimeVaryingVelocityFieldControlPointLatticeType::SizeValueType;
  using MeshSizeType = typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType;
  using DirectionType = typename TimeVaryingVelocityFieldControlPointLatticeType::DirectionType;

  /** Dimension of parameters. */
  static constexpr unsigned int TotalDimension = TransformType::Dimension + 1;

  /** Set spline order (usually from transform) */
  itkSetMacro(SplineOrder, SizeValueType);

  /** Get spline order (usually from transform) */
  itkGetConstMacro(SplineOrder, SizeValueType);

  /** Alternative method for setting the required mesh size. */
  void
  SetRequiredTransformDomainMeshSize(const MeshSizeType &);

  /** Get the required mesh size. */
  itkGetConstReferenceMacro(RequiredTransformDomainMeshSize, MeshSizeType);

  /** Alternative method for setting the required sampled size. */
  void
  SetRequiredTransformDomainSize(const SizeType &);

  /** Get the required domain size. */
  itkGetConstReferenceMacro(RequiredTransformDomainSize, SizeType);

  /** Alternative method for setting the required sampled spacing. */
  void
  SetRequiredTransformDomainSpacing(const SpacingType &);

  /** Get the required domain spacing. */
  itkGetConstReferenceMacro(RequiredTransformDomainSpacing, SpacingType);

  /** Alternative method for setting the required origin. */
  void
  SetRequiredTransformDomainOrigin(const OriginType &);

  /** Get the required origin. */
  itkGetConstReferenceMacro(RequiredTransformDomainOrigin, OriginType);

  /** Alternative method for setting the required direction. */
  void
  SetRequiredTransformDomainDirection(const DirectionType &);

  /** Get the required direction. */
  itkGetConstReferenceMacro(RequiredTransformDomainDirection, DirectionType);

  /** Get the required control point lattice origin. */
  const OriginType
  GetRequiredControlPointLatticeOrigin() const
  {
    OriginType requiredLatticeOrigin;
    for (SizeValueType i = 0; i < TotalDimension; i++)
    {
      requiredLatticeOrigin[i] = this->m_RequiredFixedParameters[TotalDimension + i];
    }
    return requiredLatticeOrigin;
  }

  /** Get the required control point lattice spacing. */
  const SpacingType
  GetRequiredControlPointLatticeSpacing() const
  {
    SpacingType requiredLatticeSpacing;
    for (SizeValueType i = 0; i < TotalDimension; i++)
    {
      FixedParametersValueType domainPhysicalDimensions =
        static_cast<FixedParametersValueType>(this->m_RequiredTransformDomainSize[i] - 1.0) *
        this->m_RequiredTransformDomainSpacing[i];
      requiredLatticeSpacing[i] =
        domainPhysicalDimensions / static_cast<FixedParametersValueType>(this->m_RequiredTransformDomainMeshSize[i]);
    }
    return requiredLatticeSpacing;
  }

  /** Get the required control point lattice size. */
  const SizeType
  GetRequiredControlPointLatticeSize() const
  {
    SizeType requiredLatticeSize;
    for (SizeValueType i = 0; i < TotalDimension; i++)
    {
      requiredLatticeSize[i] = static_cast<SizeValueType>(this->m_RequiredFixedParameters[i]);
    }
    return requiredLatticeSize;
  }

  /** Get the required control point lattice direction. */
  const DirectionType
  GetRequiredControlPointLatticeDirection() const
  {
    return this->m_RequiredTransformDomainDirection;
  }

  /** Initialize the transform using the specified fixed parameters */
  void
  AdaptTransformParameters() override;

  void
  SetRequiredFixedParameters(const FixedParametersType) override;

protected:
  TimeVaryingBSplineVelocityFieldTransformParametersAdaptor();
  ~TimeVaryingBSplineVelocityFieldTransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Helper function to set m_RequiredFixedParameters */
  void
  UpdateRequiredFixedParameters();

  MeshSizeType  m_RequiredTransformDomainMeshSize;
  OriginType    m_RequiredTransformDomainOrigin;
  DirectionType m_RequiredTransformDomainDirection;
  SpacingType   m_RequiredTransformDomainSpacing;
  SizeType      m_RequiredTransformDomainSize;

  SizeValueType m_SplineOrder;

}; // class TimeVaryingBSplineVelocityFieldTransformParametersAdaptor
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.hxx"
#endif

#endif /* itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor_h */
