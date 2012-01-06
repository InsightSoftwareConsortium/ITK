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
#ifndef __itkTimeVaryingBSplineVelocityFieldTransform_hxx
#define __itkTimeVaryingBSplineVelocityFieldTransform_hxx

#include "itkTimeVaryingBSplineVelocityFieldTransform.h"

#include "itkAddImageFilter.h"
#include "itkBSplineControlPointImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::TimeVaryingBSplineVelocityFieldTransform()
{
  this->m_TimeVaryingVelocityFieldControlPointLattice = NULL;

  this->m_SplineOrder = 3;
  this->m_TemporalPeriodicity = false;

  this->m_VelocityFieldOrigin.Fill( 0.0 );
  this->m_VelocityFieldSpacing.Fill( 1.0 );
  this->m_VelocityFieldSize.Fill( 1 );
  this->m_VelocityFieldDirection.SetIdentity();
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>::
~TimeVaryingBSplineVelocityFieldTransform()
{
}

template <class TScalar, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::SetFixedParameters( const ParametersType & fixedParameters )
{
  if( fixedParameters.Size() != TimeVaryingVelocityFieldDimension * ( TimeVaryingVelocityFieldDimension + 3 ) )
    {
    itkExceptionMacro( "The fixed parameters are not the right size." );
    }

  VelocityFieldSizeType size;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    size[d] = static_cast<SizeValueType>( fixedParameters[d] );
    }

  VelocityFieldPointType origin;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    origin[d] = fixedParameters[d + TimeVaryingVelocityFieldDimension];
    }

  VelocityFieldSpacingType spacing;
  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    spacing[d] = fixedParameters[d + 2 * TimeVaryingVelocityFieldDimension];
    }

  VelocityFieldDirectionType direction;
  for( unsigned int di = 0; di < TimeVaryingVelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < TimeVaryingVelocityFieldDimension; dj++ )
      {
      direction[di][dj] = fixedParameters[3 * TimeVaryingVelocityFieldDimension + ( di * TimeVaryingVelocityFieldDimension + dj )];
      }
    }

  DisplacementVectorType zeroDisplacement;
  zeroDisplacement.Fill( 0.0 );

  TimeVaryingVelocityFieldControlPointLatticePointer velocityFieldLattice = TimeVaryingVelocityFieldControlPointLatticeType::New();
  velocityFieldLattice->SetSpacing( spacing );
  velocityFieldLattice->SetOrigin( origin );
  velocityFieldLattice->SetDirection( direction );
  velocityFieldLattice->SetRegions( size );
  velocityFieldLattice->Allocate();
  velocityFieldLattice->FillBuffer( zeroDisplacement );

  this->SetTimeVaryingVelocityFieldControlPointLattice( velocityFieldLattice );
}

/**
 * return an inverse transformation
 */
template<class TScalar, unsigned int NDimensions>
bool
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_TimeVaryingVelocityFieldControlPointLattice )
    {
    return false;
    }
  else
    {
    inverse->SetTimeVaryingVelocityFieldControlPointLattice( this->m_TimeVaryingVelocityFieldControlPointLattice );
    inverse->SetUpperTimeBound( this->m_LowerTimeBound );
    inverse->SetLowerTimeBound( this->m_UpperTimeBound );
    inverse->SetDisplacementField( this->m_InverseDisplacementField );
    inverse->SetInverseDisplacementField( this->m_DisplacementField );
    inverse->SetInterpolator( this->m_Interpolator );
    return true;
    }
}

// Return an inverse of this transform
template<class TScalar, unsigned int NDimensions>
typename TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>::InverseTransformBasePointer
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
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
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::IntegrateVelocityField()
{
  if( !this->m_TimeVaryingVelocityFieldControlPointLattice.IsNull() )
    {
    typedef BSplineControlPointImageFilter<TimeVaryingVelocityFieldControlPointLatticeType, TimeVaryingVelocityFieldType> BSplineFilterType;

    typename BSplineFilterType::ArrayType closeDimensions;
    closeDimensions.Fill( 0 );
    if( this->m_TemporalPeriodicity )
      {
      closeDimensions[NDimensions] = 1;
      }

    typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
    bspliner->SetInput( this->m_TimeVaryingVelocityFieldControlPointLattice );
    bspliner->SetSplineOrder( this->m_SplineOrder );
    bspliner->SetSpacing( this->m_VelocityFieldSpacing );
    bspliner->SetSize( this->m_VelocityFieldSize );
    bspliner->SetDirection( this->m_VelocityFieldDirection );
    bspliner->SetOrigin( this->m_VelocityFieldOrigin );
    bspliner->SetCloseDimension( closeDimensions );
    bspliner->Update();

    typedef TimeVaryingVelocityFieldIntegrationImageFilter<TimeVaryingVelocityFieldType, DisplacementFieldType> IntegratorType;

    typename IntegratorType::Pointer integrator = IntegratorType::New();
    integrator->SetInput( bspliner->GetOutput() );
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
    inverseIntegrator->SetInput( bspliner->GetOutput() );
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
    itkExceptionMacro( "The B-spline velocity field does not exist." );
    }
}

template<class TScalar, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::SetTimeVaryingVelocityFieldControlPointLattice( TimeVaryingVelocityFieldControlPointLatticeType * fieldLattice )
{
  itkDebugMacro( "Setting TimeVaryingVelocityFieldControlPointLattice to " << fieldLattice );
  if ( this->m_TimeVaryingVelocityFieldControlPointLattice != fieldLattice )
    {
    this->m_TimeVaryingVelocityFieldControlPointLattice = fieldLattice;
    this->Modified();
    // Assign to parameters object
    this->m_Parameters.SetParametersObject( this->m_TimeVaryingVelocityFieldControlPointLattice );
    }
}

template<class TScalar, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType & update, ScalarType factor )
{
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro( "Parameter update size, " << update.Size()
      << ", must be same as transform parameter size, "
      << numberOfParameters << std::endl );
    }

  DerivativeType scaledUpdate = update;
  scaledUpdate *= factor;

  const SizeValueType numberOfPixels = static_cast<SizeValueType>( scaledUpdate.Size() / NDimensions );
  const bool importFilterWillReleaseMemory = false;

  DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( scaledUpdate.data_block() );

  typedef ImportImageFilter<DisplacementVectorType, NDimensions+1> ImporterType;
  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( this->m_TimeVaryingVelocityFieldControlPointLattice->GetBufferedRegion() );
  importer->SetOrigin( this->m_TimeVaryingVelocityFieldControlPointLattice->GetOrigin() );
  importer->SetSpacing( this->m_TimeVaryingVelocityFieldControlPointLattice->GetSpacing() );
  importer->SetDirection( this->m_TimeVaryingVelocityFieldControlPointLattice->GetDirection() );
  importer->Update();

  typedef AddImageFilter<TimeVaryingVelocityFieldControlPointLatticeType,
    TimeVaryingVelocityFieldControlPointLatticeType, TimeVaryingVelocityFieldControlPointLatticeType> AdderType;
  typename AdderType::Pointer adder = AdderType::New();
  adder->SetInput1( this->m_TimeVaryingVelocityFieldControlPointLattice );
  adder->SetInput2( importer->GetOutput() );

  TimeVaryingVelocityFieldControlPointLatticePointer totalFieldLattice = adder->GetOutput();
  totalFieldLattice->Update();

  this->SetTimeVaryingVelocityFieldControlPointLattice( totalFieldLattice );
}

template <class TScalar, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TScalar, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;

  os << indent << "Sampled velocity field parameters" << std::endl;
  os << indent << "  size: " << this->m_VelocityFieldSize << std::endl;
  os << indent << "  spacing: " << this->m_VelocityFieldSpacing << std::endl;
  os << indent << "  origin: " << this->m_VelocityFieldOrigin << std::endl;
  os << indent << "  direction: " << this->m_VelocityFieldDirection << std::endl;
}
} // namespace itk

#endif
