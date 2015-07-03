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
#ifndef itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor_hxx
#define itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor_hxx

#include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.h"

#include "itkBSplineDecompositionImageFilter.h"
#include "itkBSplineResampleImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkResampleImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"

namespace itk
{

template<typename TTransform>
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor()
{
  this->m_RequiredFixedParameters.SetSize( TotalDimension * ( TotalDimension + 4 ) );

  this->m_RequiredTransformDomainOrigin.Fill( 0.0 );
  this->m_RequiredTransformDomainDirection.SetIdentity();
  this->m_RequiredTransformDomainSize.Fill( 1 );
  this->m_RequiredTransformDomainSpacing.Fill( 0.0 );
  this->m_RequiredTransformDomainMeshSize.Fill( 1 );

  this->m_SplineOrder = 3;

  this->UpdateRequiredFixedParameters();
}

template<typename TTransform>
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::~TimeVaryingBSplineVelocityFieldTransformParametersAdaptor()
{
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainOrigin( const OriginType & origin )
{
  if( origin != this->m_RequiredTransformDomainOrigin )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainOrigin to " << origin );
    this->m_RequiredTransformDomainOrigin = origin;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainSize( const SizeType & size )
{
  if( size != this->m_RequiredTransformDomainSize )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainSize to " << size );
    this->m_RequiredTransformDomainSize = size;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainSpacing( const SpacingType & spacing )
{
  if( spacing != this->m_RequiredTransformDomainSpacing )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainSpacing to " << spacing );
    this->m_RequiredTransformDomainSpacing = spacing;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainDirection( const DirectionType & direction )
{
  if( direction != this->m_RequiredTransformDomainDirection )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainDirection to " << direction );
    this->m_RequiredTransformDomainDirection = direction;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainMeshSize( const MeshSizeType & meshSize )
{
  if( meshSize != this->m_RequiredTransformDomainMeshSize )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainMeshSize to " << meshSize );
    this->m_RequiredTransformDomainMeshSize = meshSize;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::UpdateRequiredFixedParameters()
{
  // Fixed parameters store the following information:
  //  grid size
  //  grid origin
  //  transform domain sampled size
  //  transform domain sampled spacing
  //  grid direction

  // set the grid size parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredFixedParameters[i] = this->m_RequiredTransformDomainMeshSize[i] + this->m_SplineOrder;
    }

  // Set the origin parameters
  OriginType origin;
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    FixedParametersValueType domainPhysicalDimensions = ( this->m_RequiredTransformDomainSize[i] - 1.0 ) * this->m_RequiredTransformDomainSpacing[i];

    FixedParametersValueType gridSpacing = domainPhysicalDimensions /
      static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainMeshSize[i] );
    origin[i] = -0.5 * gridSpacing * ( this->m_SplineOrder - 1 );
    }
  origin = this->m_RequiredTransformDomainDirection * origin;
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredFixedParameters[TotalDimension + i] = static_cast<FixedParametersValueType>(
      origin[i] + this->m_RequiredTransformDomainOrigin[i] );
    }

  // Set the domain sampled size parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredFixedParameters[2 * TotalDimension + i] =
      static_cast<SizeValueType>( this->m_RequiredTransformDomainSize[i] );
    }

  // Set the domain sampled spacing parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredFixedParameters[3 * TotalDimension + i] =
      static_cast<ParametersValueType>( this->m_RequiredTransformDomainSpacing[i] );
    }

  // Set the direction parameters
  for( SizeValueType di = 0; di < TotalDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < TotalDimension; dj++ )
      {
      this->m_RequiredFixedParameters[4 * TotalDimension + ( di * TotalDimension + dj )] =
        static_cast<ParametersValueType>( this->m_RequiredTransformDomainDirection[di][dj] );
      }
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::SetRequiredFixedParameters( const FixedParametersType fixedParameters )
{
  Superclass::SetRequiredFixedParameters( fixedParameters );

  // Set the direction parameters
  for( SizeValueType di = 0; di < TotalDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < TotalDimension; dj++ )
      {
      this->m_RequiredTransformDomainDirection[di][dj] =
        this->m_RequiredFixedParameters[4 * TotalDimension + ( di * TotalDimension + dj )];
      }
    }

  // set the mesh size parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredTransformDomainMeshSize[i] =
      static_cast<SizeValueType>( this->m_RequiredFixedParameters[i] ) - this->m_SplineOrder;
    }

  // Set the origin parameters
  OriginType origin;
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    FixedParametersValueType domainPhysicalDimensions = static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainSize[i] - 1.0 ) *
      this->m_RequiredTransformDomainSpacing[i];
    FixedParametersValueType gridSpacing = domainPhysicalDimensions / static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainMeshSize[i] );
    origin[i] = 0.5 * gridSpacing * ( this->m_SplineOrder - 1 );
    }
  origin = this->m_RequiredTransformDomainDirection * origin;
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredTransformDomainOrigin[i] = origin[i] + this->m_RequiredFixedParameters[TotalDimension + i];
    }

  // Set the domain sampled size parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredTransformDomainSize[i] =
      static_cast<SizeValueType>( this->m_RequiredFixedParameters[2 * TotalDimension + i] );
    }

  // Set the domain sampled spacing parameters
  for( SizeValueType i = 0; i < TotalDimension; i++ )
    {
    this->m_RequiredTransformDomainSpacing[i] =
      static_cast<ParametersValueType>( this->m_RequiredFixedParameters[3 * TotalDimension + i] );
    }
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  if( !this->m_Transform )
    {
    itkExceptionMacro( "Transform has not been set." );
    }

  if( this->m_RequiredFixedParameters == this->m_Transform->GetFixedParameters() )
    {
    return;
    }

  SizeType requiredLatticeSize = this->GetRequiredControlPointLatticeSize();
  OriginType requiredLatticeOrigin = this->GetRequiredControlPointLatticeOrigin();
  SpacingType requiredLatticeSpacing = this->GetRequiredControlPointLatticeSpacing();
  DirectionType requiredLatticeDirection = this->GetRequiredControlPointLatticeDirection();

  const RegionType & latticeRegion =
    this->m_Transform->GetTimeVaryingVelocityFieldControlPointLattice()->GetLargestPossibleRegion();
  IndexType requiredLatticeIndex = latticeRegion.GetIndex();

  typedef Image<ParametersValueType, TotalDimension> ComponentImageType;

  //  Resample the coefficient images
  typedef BSplineResampleImageFunction<ComponentImageType, ParametersValueType> CoefficientUpsampleFunctionType;
  typedef ResampleImageFilter<ComponentImageType, ComponentImageType, ParametersValueType> UpsampleFilterType;
  typedef BSplineDecompositionImageFilter<ComponentImageType, ComponentImageType> DecompositionFilterType;

  VectorType zeroVector( 0.0 );

  TimeVaryingVelocityFieldControlPointLatticePointer requiredLattice = TimeVaryingVelocityFieldControlPointLatticeType::New();
  requiredLattice->SetRegions( requiredLatticeSize );
  requiredLattice->SetOrigin( requiredLatticeOrigin );
  requiredLattice->SetSpacing( requiredLatticeSpacing );
  requiredLattice->SetDirection( requiredLatticeDirection );
  requiredLattice->Allocate();
  requiredLattice->FillBuffer( zeroVector );

  // Loop over dimension: each direction is upsampled separately.
  for( SizeValueType j = 0; j < TotalDimension-1; j++ )
    {
    typedef VectorIndexSelectionCastImageFilter<TimeVaryingVelocityFieldControlPointLatticeType, ComponentImageType> SelectionFilterType;
    typename SelectionFilterType::Pointer selector = SelectionFilterType::New();
    selector->SetInput( this->m_Transform->GetTimeVaryingVelocityFieldControlPointLattice() );
    selector->SetIndex( j );

    /* Set the coeficient image as the input of the upsampler filter.
     * The upsampler samples the deformation field at the locations
     * of the new control points, given the current coefficients
     * (note: it does not just interpolate the coefficient image,
     * which would be wrong). The B-spline coefficients that
     * describe the resulting image are computed by the
     * decomposition filter.
     *
     * This code is copied from the itk-example
     * DeformableRegistration6.cxx .
     */
    typename UpsampleFilterType::Pointer upsampler = UpsampleFilterType::New();
    typename CoefficientUpsampleFunctionType::Pointer coeffUpsampleFunction = CoefficientUpsampleFunctionType::New();

    upsampler->SetInterpolator( coeffUpsampleFunction );
    upsampler->SetSize( requiredLatticeSize );
    upsampler->SetOutputStartIndex( requiredLatticeIndex );
    upsampler->SetOutputSpacing( requiredLatticeSpacing );
    upsampler->SetOutputOrigin( requiredLatticeOrigin );
    upsampler->SetOutputDirection( requiredLatticeDirection );
    upsampler->SetInput( selector->GetOutput() );

    typename DecompositionFilterType::Pointer decompositionFilter = DecompositionFilterType::New();
    decompositionFilter->SetSplineOrder( this->m_Transform->GetSplineOrder() );
    decompositionFilter->SetInput( upsampler->GetOutput() );
    decompositionFilter->Update();

    ImageRegionConstIterator<ComponentImageType> ItD( decompositionFilter->GetOutput(), decompositionFilter->GetOutput()->GetLargestPossibleRegion() );
    ImageRegionIterator<TimeVaryingVelocityFieldControlPointLatticeType> ItL( requiredLattice, requiredLattice->GetLargestPossibleRegion() );

    for( ItD.GoToBegin(), ItL.GoToBegin(); !ItD.IsAtEnd(); ++ItD, ++ItL )
      {
      typename TimeVaryingVelocityFieldControlPointLatticeType::PixelType velocity = ItL.Get();
      velocity[j] = ItD.Get();
      ItL.Set( velocity );
      }
    }

  this->m_Transform->SetTimeVaryingVelocityFieldControlPointLattice( requiredLattice );
  this->m_Transform->SetVelocityFieldOrigin( this->m_RequiredTransformDomainOrigin );
  this->m_Transform->SetVelocityFieldSpacing( this->m_RequiredTransformDomainSpacing );
  this->m_Transform->SetVelocityFieldSize( this->m_RequiredTransformDomainSize );
  this->m_Transform->SetVelocityFieldDirection( this->m_RequiredTransformDomainDirection );
  this->m_Transform->SetLowerTimeBound( 0.0 );
  this->m_Transform->SetUpperTimeBound( 1.0 );
  this->m_Transform->IntegrateVelocityField();
}

template<typename TTransform>
void
TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << "Required transform domain origin: " << this->m_RequiredTransformDomainOrigin << std::endl;
  os << "Required transform domain mesh size: " << this->m_RequiredTransformDomainMeshSize << std::endl;
  os << "Required transform domain sampled size: " << this->m_RequiredTransformDomainSize << std::endl;
  os << "Required transform domain sampled spacing: " << this->m_RequiredTransformDomainSpacing << std::endl;
  os << "Required transform domain direction: " << this->m_RequiredTransformDomainDirection << std::endl;
}

}  // namespace itk

#endif
