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
#ifndef __itkTimeVaryingVelocityFieldTransform_hxx
#define __itkTimeVaryingVelocityFieldTransform_hxx

#include "itkTimeVaryingVelocityFieldTransform.h"

#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::TimeVaryingVelocityFieldTransform()
{
  this->m_LowerTimeBound = 0.0;
  this->m_UpperTimeBound = 1.0;
  this->m_NumberOfIntegrationSteps = 100;

  this->m_TimeVaryingVelocityField = NULL;

  // Setup and assign parameter helper. This will hold the time varying velocity
  // field for access through the common OptimizerParameters interface.
  OptimizerParametersHelperType * helper = new OptimizerParametersHelperType;

  // After assigning this, parameters will manage this deleting when appropriate.
  this->m_Parameters.SetHelper( helper );

  typedef VectorLinearInterpolateImageFunction
    <TimeVaryingVelocityFieldType, ScalarType> DefaultInterpolatorType;

  this->m_TimeVaryingVelocityFieldInterpolator = DefaultInterpolatorType::New();
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>::
~TimeVaryingVelocityFieldTransform()
{
}

template <class TScalar, unsigned int NDimensions>
void
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::SetFixedParameters( const ParametersType & fixedParameters )
{
  if( fixedParameters.Size() != TimeVaryingVelocityFieldDimension * ( TimeVaryingVelocityFieldDimension + 3 ) )
    {
    itkExceptionMacro( "The fixed parameters are not the right size." );
    }

  SizeType size;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    size[d] = static_cast<SizeValueType>( fixedParameters[d] );
    }

  PointType origin;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    origin[d] = fixedParameters[d + TimeVaryingVelocityFieldDimension];
    }

  SpacingType spacing;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    spacing[d] = fixedParameters[d + 2 * TimeVaryingVelocityFieldDimension];
    }

  DirectionType direction;
  for( unsigned int di = 0; di < TimeVaryingVelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < TimeVaryingVelocityFieldDimension; dj++ )
      {
      direction[di][dj] = fixedParameters[3 * TimeVaryingVelocityFieldDimension + ( di * TimeVaryingVelocityFieldDimension + dj )];
      }
    }

  DisplacementVectorType zeroDisplacement;
  zeroDisplacement.Fill( 0.0 );

  TimeVaryingVelocityFieldPointer velocityField = TimeVaryingVelocityFieldType::New();
  velocityField->SetSpacing( spacing );
  velocityField->SetOrigin( origin );
  velocityField->SetDirection( direction );
  velocityField->SetRegions( size );
  velocityField->Allocate();
  velocityField->FillBuffer( zeroDisplacement );

  this->SetTimeVaryingVelocityField( velocityField );
}

/**
 * return an inverse transformation
 */
template<class TScalar, unsigned int NDimensions>
bool
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_TimeVaryingVelocityField )
    {
    return false;
    }
  else
    {
    inverse->SetTimeVaryingVelocityField( this->m_TimeVaryingVelocityField );
    inverse->SetUpperTimeBound( this->m_LowerTimeBound );
    inverse->SetLowerTimeBound( this->m_UpperTimeBound );
    inverse->SetTimeVaryingVelocityFieldInterpolator( this->m_TimeVaryingVelocityFieldInterpolator );
    inverse->SetDisplacementField( this->m_InverseDisplacementField );
    inverse->SetInverseDisplacementField( this->m_DisplacementField );
    inverse->SetInterpolator( this->m_Interpolator );
    return true;
    }
}

// Return an inverse of this transform
template<class TScalar, unsigned int NDimensions>
typename TimeVaryingVelocityFieldTransform<TScalar, NDimensions>::
  InverseTransformBasePointer
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::GetInverseTransform() const
{
  Pointer inverseTransform = New();
  if( this->GetInverse( inverseTransform ) )
    {
    return inverseTransform.GetPointer();
    }
  else
    {
    return NULL;
    }
}

template<class TScalar, unsigned int NDimensions>
void
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::IntegrateVelocityField()
{
  if( !this->m_TimeVaryingVelocityField.IsNull() )
    {
    typedef TimeVaryingVelocityFieldIntegrationImageFilter
      <TimeVaryingVelocityFieldType, DisplacementFieldType> IntegratorType;

    typename IntegratorType::Pointer integrator = IntegratorType::New();
    integrator->SetInput( this->m_TimeVaryingVelocityField );
    integrator->SetLowerTimeBound( this->m_LowerTimeBound );
    integrator->SetUpperTimeBound( this->m_UpperTimeBound );

    if( !this->m_TimeVaryingVelocityFieldInterpolator.IsNull() )
      {
      integrator->SetVelocityFieldInterpolator( this->m_TimeVaryingVelocityFieldInterpolator );
      }

    integrator->SetNumberOfIntegrationSteps( this->m_NumberOfIntegrationSteps );
    integrator->Update();

    typename DisplacementFieldType::Pointer displacementField = integrator->GetOutput();
    displacementField->DisconnectPipeline();

    this->SetDisplacementField( displacementField );
    this->GetInterpolator()->SetInputImage( displacementField );

    typename IntegratorType::Pointer inverseIntegrator = IntegratorType::New();
    inverseIntegrator->SetInput( this->m_TimeVaryingVelocityField );
    inverseIntegrator->SetLowerTimeBound( this->m_UpperTimeBound );
    inverseIntegrator->SetUpperTimeBound( this->m_LowerTimeBound );
    if( !this->m_TimeVaryingVelocityFieldInterpolator.IsNull() )
      {
      inverseIntegrator->SetVelocityFieldInterpolator( this->m_TimeVaryingVelocityFieldInterpolator );
      }

    inverseIntegrator->SetNumberOfIntegrationSteps( this->m_NumberOfIntegrationSteps );
    inverseIntegrator->Update();

    typename DisplacementFieldType::Pointer inverseDisplacementField = inverseIntegrator->GetOutput();
    inverseDisplacementField->DisconnectPipeline();

    this->SetInverseDisplacementField( inverseDisplacementField );
    }
  else
    {
    itkExceptionMacro( "The velocity field does not exist." );
    }
}

template<class TScalar, unsigned int NDimensions>
void
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::SetParameters( const ParametersType & params )
{
  if( &(this->m_Parameters) != &params )
    {
    if( params.Size() != this->m_Parameters.Size() )
      {
      itkExceptionMacro( "Input parameters size (" << params.Size()
        << ") does not match internal size ("
        << this->m_Parameters.Size() << ")." );
      }
    /* copy into existing object */
    this->m_Parameters = params;
    this->Modified();
    }
}

template<class TScalar, unsigned int NDimensions>
void
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType & update, ScalarType factor )
{
  //This simply adds the values.
  //TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters( update, factor );

  this->IntegrateVelocityField();
}

template<class TScalar, unsigned int NDimensions>
void TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::SetTimeVaryingVelocityField( TimeVaryingVelocityFieldType * field )
{
  itkDebugMacro( "Setting TimeVaryingVelocityField to " << field );
  if ( this->m_TimeVaryingVelocityField != field )
    {
    this->m_TimeVaryingVelocityField = field;
    this->Modified();
    if( !this->m_TimeVaryingVelocityFieldInterpolator.IsNull() )
      {
      this->m_TimeVaryingVelocityFieldInterpolator->SetInputImage(
        this->m_TimeVaryingVelocityField );
      }
    // Assign to parameters object
    this->m_Parameters.SetParametersObject( this->m_TimeVaryingVelocityField );
    }
}

template <class TScalar, unsigned int NDimensions>
void
TimeVaryingVelocityFieldTransform<TScalar, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  std::cout << indent << "TimeVaryingVelocityFieldInterpolator: " << std::endl;
  std::cout << indent << indent << this->m_TimeVaryingVelocityFieldInterpolator
    << std::endl;

  std::cout << indent << "TimeVaryingVelocityField: " << std::endl;
  std::cout << indent << indent << this->m_TimeVaryingVelocityField
    << std::endl;

  os << indent << "LowerTimeBound: " << this->m_LowerTimeBound << std::endl;
  os << indent << "UpperTimeBound: " << this->m_UpperTimeBound << std::endl;
  os << indent << "NumberOfIntegrationSteps: "
    << this->m_NumberOfIntegrationSteps << std::endl;
}
} // namespace itk

#endif
