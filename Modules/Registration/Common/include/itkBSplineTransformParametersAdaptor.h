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
#ifndef itkBSplineTransformParametersAdaptor_h
#define itkBSplineTransformParametersAdaptor_h

#include "itkTransformParametersAdaptor.h"

namespace itk
{
/** \class BSplineTransformParametersAdaptor
 * \brief BSplineTransformParametersAdaptor adapts a BSplineTransform to the
 * new specified fixed parameters.
 *
 * The fixed parameters of the BSplineTransform store the following information
 * (in order as they appear in m_FixedParameters):
 *    grid size
 *    grid origin
 *    grid spacing
 *    grid direction
 *
 * During multiresolution image registration it is often desired to also increase
 * the B-spline grid resolution for greater flexibility in optimizing the
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
 * extent but only increase the mesh size.  This can be done as follows:
 *
     \code
     transformAdaptor->SetTransform( transform );
     transformAdaptor->SetRequiredTransformDomainOrigin( transform->GetTransformDomainOrigin() );
     transformAdaptor->SetRequiredTransformDomainDirection( transform->GetTransformDomainDirection() );
     transformAdaptor->SetRequiredTransformDomainPhysicalDimensions( transform->GetTransformDomainPhysicalDimensions()
 ); transformAdaptor->SetRequiredTransformDomainMeshSize( newMeshSize ); transformAdaptor->AdaptTransformParameters();
     \endcode
 *
 * \author Nick Tustison
 * \author Marius Staring
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class ITK_TEMPLATE_EXPORT BSplineTransformParametersAdaptor : public TransformParametersAdaptor<TTransform>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineTransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = BSplineTransformParametersAdaptor;
  using Superclass = TransformParametersAdaptor<TTransform>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineTransformParametersAdaptor, TransformParametersAdaptor);

  /** Typedefs associated with the transform */
  using TransformType = TTransform;
  using TransformPointer = typename TransformType::Pointer;

  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;

  using OriginType = typename TransformType::OriginType;
  using SizeType = typename TransformType::SizeType;
  using SpacingType = typename TransformType::SpacingType;
  using IndexType = typename TransformType::IndexType;
  using MeshSizeType = typename TransformType::MeshSizeType;
  using DirectionType = typename TransformType::DirectionType;
  using PhysicalDimensionsType = typename TransformType::PhysicalDimensionsType;


  using ImageType = typename TransformType::ImageType;
  using RegionType = typename ImageType::RegionType;
  using CoefficientImageArray = typename TransformType::CoefficientImageArray;

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = TransformType::SpaceDimension;

  /** Alternative method for setting the required mesh size. */
  void
  SetRequiredTransformDomainMeshSize(const MeshSizeType &);

  /** Get the required mesh size. */
  itkGetConstReferenceMacro(RequiredTransformDomainMeshSize, MeshSizeType);

  /** Alternative method for setting the required mesh size. */
  void
  SetRequiredTransformDomainPhysicalDimensions(const PhysicalDimensionsType &);

  /** Get the required physical dimensions. */
  itkGetConstReferenceMacro(RequiredTransformDomainPhysicalDimensions, PhysicalDimensionsType);

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

  void
  SetRequiredFixedParameters(const FixedParametersType) override;

  /** Initialize the transform using the specified fixed parameters */
  void
  AdaptTransformParameters() override;

protected:
  BSplineTransformParametersAdaptor();
  ~BSplineTransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Helper function to set m_RequiredFixedParameters */
  void
  UpdateRequiredFixedParameters();

  MeshSizeType           m_RequiredTransformDomainMeshSize;
  OriginType             m_RequiredTransformDomainOrigin;
  DirectionType          m_RequiredTransformDomainDirection;
  PhysicalDimensionsType m_RequiredTransformDomainPhysicalDimensions;

}; // class BSplineTransformParametersAdaptor
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineTransformParametersAdaptor.hxx"
#endif

#endif /* itkBSplineTransformParametersAdaptor_h */
