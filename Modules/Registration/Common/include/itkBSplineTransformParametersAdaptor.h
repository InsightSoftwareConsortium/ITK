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
 *   \code
 *   transformAdaptor->SetTransform( transform );
 *   transformAdaptor->SetRequiredFixedParameters( fixedParameters );
 *   transformAdaptor->AdaptTransformParameters();
 *   \endcode
 *
 * or the user can use the more intuitive API for setting the fixed parameters.
 * E.g., often the user will want to maintain the same transform domain spatial
 * extent but only increase the mesh size.  This can be done as follows:
 *
 *   \code
 *   transformAdaptor->SetTransform( transform );
 *   transformAdaptor->SetRequiredTransformDomainOrigin( transform->GetTransformDomainOrigin() );
 *   transformAdaptor->SetRequiredTransformDomainDirection( transform->GetTransformDomainDirection() );
 *   transformAdaptor->SetRequiredTransformDomainPhysicalDimensions( transform->GetTransformDomainPhysicalDimensions() );
 *   transformAdaptor->SetRequiredTransformDomainMeshSize( newMeshSize );
 *   transformAdaptor->AdaptTransformParameters();
 *   \endcode
 *
 * \author Nick Tustison
 * \author Marius Staring
 *
 * \ingroup ITKRegistrationCommon
 */
template<typename TTransform>
class ITK_TEMPLATE_EXPORT BSplineTransformParametersAdaptor
: public TransformParametersAdaptor<TTransform>
{
public:

  /** Standard class typedefs. */
  typedef BSplineTransformParametersAdaptor          Self;
  typedef TransformParametersAdaptor<TTransform>     Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineTransformParametersAdaptor, TransformParametersAdaptor );

  /** Typedefs associated with the transform */
  typedef TTransform                                        TransformType;
  typedef typename TransformType::Pointer                   TransformPointer;

  typedef typename Superclass::FixedParametersType          FixedParametersType;
  typedef typename Superclass::FixedParametersValueType     FixedParametersValueType;
  typedef typename Superclass::ParametersType               ParametersType;
  typedef typename Superclass::ParametersValueType          ParametersValueType;

  typedef typename TransformType::OriginType                OriginType;
  typedef typename TransformType::SizeType                  SizeType;
  typedef typename TransformType::SpacingType               SpacingType;
  typedef typename TransformType::IndexType                 IndexType;
  typedef typename TransformType::MeshSizeType              MeshSizeType;
  typedef typename TransformType::DirectionType             DirectionType;
  typedef typename TransformType::PhysicalDimensionsType    PhysicalDimensionsType;


  typedef typename TransformType::ImageType                 ImageType;
  typedef typename ImageType::RegionType                    RegionType;
  typedef typename TransformType::CoefficientImageArray     CoefficientImageArray;

  /** Dimension of parameters. */
  itkStaticConstMacro( SpaceDimension, unsigned int, TransformType::SpaceDimension );

  /** Alternative method for setting the required mesh size. */
  void SetRequiredTransformDomainMeshSize( const MeshSizeType & );

  /** Get the required mesh size. */
  itkGetConstReferenceMacro( RequiredTransformDomainMeshSize, MeshSizeType );

  /** Alternative method for setting the required mesh size. */
  void SetRequiredTransformDomainPhysicalDimensions( const PhysicalDimensionsType & );

  /** Get the required physical dimensions. */
  itkGetConstReferenceMacro( RequiredTransformDomainPhysicalDimensions, PhysicalDimensionsType );

  /** Alternative method for setting the required origin. */
  void SetRequiredTransformDomainOrigin( const OriginType & );

  /** Get the required origin. */
  itkGetConstReferenceMacro( RequiredTransformDomainOrigin, OriginType );

  /** Alternative method for setting the required direction. */
  void SetRequiredTransformDomainDirection( const DirectionType & );

  /** Get the required direction. */
  itkGetConstReferenceMacro( RequiredTransformDomainDirection, DirectionType );

  virtual void SetRequiredFixedParameters( const FixedParametersType ) ITK_OVERRIDE;

  /** Initialize the transform using the specified fixed parameters */
  virtual void AdaptTransformParameters() ITK_OVERRIDE;

protected:
  BSplineTransformParametersAdaptor();
  ~BSplineTransformParametersAdaptor() ITK_OVERRIDE;

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineTransformParametersAdaptor);

  /** Helper function to set m_RequiredFixedParameters */
  void UpdateRequiredFixedParameters();

  MeshSizeType                               m_RequiredTransformDomainMeshSize;
  OriginType                                 m_RequiredTransformDomainOrigin;
  DirectionType                              m_RequiredTransformDomainDirection;
  PhysicalDimensionsType                     m_RequiredTransformDomainPhysicalDimensions;

}; //class BSplineTransformParametersAdaptor
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineTransformParametersAdaptor.hxx"
#endif

#endif /* itkBSplineTransformParametersAdaptor_h */
