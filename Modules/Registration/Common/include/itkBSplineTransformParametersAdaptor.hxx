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
#ifndef itkBSplineTransformParametersAdaptor_hxx
#define itkBSplineTransformParametersAdaptor_hxx

#include "itkBSplineTransformParametersAdaptor.h"

#include "itkBSplineDecompositionImageFilter.h"
#include "itkBSplineResampleImageFunction.h"
#include "itkResampleImageFilter.h"

namespace itk
{

template<typename TTransform>
BSplineTransformParametersAdaptor<TTransform>
::BSplineTransformParametersAdaptor()
{
  this->m_RequiredFixedParameters.SetSize( SpaceDimension * ( SpaceDimension + 3 ) );

  this->m_RequiredTransformDomainOrigin.Fill( 0.0 );
  this->m_RequiredTransformDomainDirection.SetIdentity();
  this->m_RequiredTransformDomainPhysicalDimensions.Fill( 1.0 );
  this->m_RequiredTransformDomainMeshSize.Fill( 1 );

  this->UpdateRequiredFixedParameters();
}

template<typename TTransform>
BSplineTransformParametersAdaptor<TTransform>
::~BSplineTransformParametersAdaptor()
{
}

template<typename TTransform>
void
BSplineTransformParametersAdaptor<TTransform>
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
BSplineTransformParametersAdaptor<TTransform>
::SetRequiredTransformDomainPhysicalDimensions( const PhysicalDimensionsType & dimensions )
{
  if( dimensions != this->m_RequiredTransformDomainPhysicalDimensions )
    {
    itkDebugMacro( "Setting m_RequiredTransformDomainPhysicalDimensions to " << dimensions );
    this->m_RequiredTransformDomainPhysicalDimensions = dimensions;
    this->UpdateRequiredFixedParameters();

    this->Modified();
    }
}

template<typename TTransform>
void
BSplineTransformParametersAdaptor<TTransform>
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
BSplineTransformParametersAdaptor<TTransform>
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
BSplineTransformParametersAdaptor<TTransform>
::SetRequiredFixedParameters( const FixedParametersType fixedParameters )
{
  Superclass::SetRequiredFixedParameters( fixedParameters );

  // Set the direction parameters
  for( SizeValueType di = 0; di < SpaceDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < SpaceDimension; dj++ )
      {
      this->m_RequiredTransformDomainDirection[di][dj] =
        this->m_RequiredFixedParameters[3 * SpaceDimension + ( di * SpaceDimension + dj )];
      }
    }

  // set the mesh size parameters
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    this->m_RequiredTransformDomainMeshSize[i] = static_cast<SizeValueType>( this->m_RequiredFixedParameters[i] ) - TransformType::SplineOrder;
    }

  // Set the physical dimensions parameters
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    FixedParametersValueType gridSpacing = this->m_RequiredFixedParameters[2 * SpaceDimension + i];
    this->m_RequiredTransformDomainPhysicalDimensions[i] = gridSpacing *
      static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainMeshSize[i] );
    }

  // Set the origin parameters
  OriginType origin;
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    FixedParametersValueType gridSpacing = this->m_RequiredFixedParameters[2 * SpaceDimension + i];
    origin[i] = 0.5 * gridSpacing * ( TransformType::SplineOrder - 1 );
    }
  origin = this->m_RequiredTransformDomainDirection * origin;
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    this->m_RequiredTransformDomainOrigin[i] = origin[i] + this->m_RequiredFixedParameters[SpaceDimension + i];
    }
}


template<typename TTransform>
void
BSplineTransformParametersAdaptor<TTransform>
::UpdateRequiredFixedParameters()
{
  // Fixed parameters store the following information:
  //  grid size
  //  grid origin
  //  grid spacing
  //  grid direction
  //  The size of these is equal to the SpaceDimension

  // set the grid size parameters
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    this->m_RequiredFixedParameters[i] = this->m_RequiredTransformDomainMeshSize[i] + TransformType::SplineOrder;
    }

  // Set the origin parameters
  OriginType origin;
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    FixedParametersValueType gridSpacing = this->m_RequiredTransformDomainPhysicalDimensions[i] /
      static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainMeshSize[i] );
    origin[i] = -0.5 * gridSpacing * ( TransformType::SplineOrder - 1 );
    }
  origin = this->m_RequiredTransformDomainDirection * origin;
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    this->m_RequiredFixedParameters[SpaceDimension + i] = static_cast<FixedParametersValueType>(
      origin[i] + this->m_RequiredTransformDomainOrigin[i] );
    }

  // Set the spacing parameters
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    FixedParametersValueType gridSpacing = this->m_RequiredTransformDomainPhysicalDimensions[i] /
      static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainMeshSize[i] );
    this->m_RequiredFixedParameters[2 * SpaceDimension + i] =
      static_cast<FixedParametersValueType>( gridSpacing );
    }

  // Set the direction parameters
  for( SizeValueType di = 0; di < SpaceDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < SpaceDimension; dj++ )
      {
      this->m_RequiredFixedParameters[3 * SpaceDimension + ( di * SpaceDimension + dj )] =
        static_cast<FixedParametersValueType>( this->m_RequiredTransformDomainDirection[di][dj] );
      }
    }
}

template<typename TTransform>
void
BSplineTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  if( !this->m_Transform )
    {
    itkExceptionMacro( "Transform has not been set." );
    return;
    }

  if( this->m_RequiredFixedParameters == this->m_Transform->GetFixedParameters() )
    {
    return;
    }

  SizeType newGridSize;
  OriginType newGridOrigin;
  SpacingType newGridSpacing;
  DirectionType newGridDirection;
  for( SizeValueType i = 0; i < SpaceDimension; i++ )
    {
    newGridSize[i] = static_cast<SizeValueType>( this->m_RequiredFixedParameters[i] );
    newGridOrigin[i] = this->m_RequiredFixedParameters[SpaceDimension + i];
    newGridSpacing[i] = this->m_RequiredFixedParameters[2 * SpaceDimension + i];
    for( SizeValueType j = 0; j < SpaceDimension; j++ )
      {
      newGridDirection[i][j] = this->m_RequiredFixedParameters[3 * SpaceDimension + ( i * SpaceDimension + j )];
      }
    }

  const RegionType & coefficientImageRegion =
    this->m_Transform->GetCoefficientImages()[0]->GetLargestPossibleRegion();
  IndexType newGridIndex = coefficientImageRegion.GetIndex();

  //  Resample the coefficient images
  typedef BSplineResampleImageFunction<ImageType, ParametersValueType> CoefficientUpsampleFunctionType;
  typedef ResampleImageFilter<ImageType, ImageType, ParametersValueType> UpsampleFilterType;
  typedef BSplineDecompositionImageFilter<ImageType, ImageType> DecompositionFilterType;

  CoefficientImageArray newCoefficientImages;

  // Loop over dimension: each direction is upsampled separately.
  for( SizeValueType j = 0; j < SpaceDimension; j++ )
    {
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
    typename CoefficientUpsampleFunctionType::Pointer coeffUpsampleFunction =
      CoefficientUpsampleFunctionType::New();

    upsampler->SetInterpolator( coeffUpsampleFunction );
    upsampler->SetSize( newGridSize );
    upsampler->SetOutputStartIndex( newGridIndex );
    upsampler->SetOutputSpacing( newGridSpacing );
    upsampler->SetOutputOrigin( newGridOrigin );
    upsampler->SetOutputDirection( newGridDirection );
    upsampler->SetInput( this->m_Transform->GetCoefficientImages()[j] );

    typename DecompositionFilterType::Pointer decompositionFilter = DecompositionFilterType::New();
    decompositionFilter->SetSplineOrder( TransformType::SplineOrder );
    decompositionFilter->SetInput( upsampler->GetOutput() );

    newCoefficientImages[j] = decompositionFilter->GetOutput();
    newCoefficientImages[j]->Update();
    newCoefficientImages[j]->DisconnectPipeline();
    }

  this->m_Transform->SetCoefficientImages( newCoefficientImages );
}

template<typename TTransform>
void
BSplineTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << "Required transform domain origin: " << this->m_RequiredTransformDomainOrigin << std::endl;
  os << "Required transform domain direction: " << this->m_RequiredTransformDomainDirection << std::endl;
  os << "Required transform domain physical dimensions: " << this->m_RequiredTransformDomainPhysicalDimensions << std::endl;
  os << "Required transform domain mesh size: " << this->m_RequiredTransformDomainMeshSize << std::endl;
}

}  // namespace itk

#endif
