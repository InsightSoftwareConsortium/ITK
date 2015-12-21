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
#ifndef itkTimeVaryingBSplineVelocityFieldTransform_hxx
#define itkTimeVaryingBSplineVelocityFieldTransform_hxx

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
template<typename TParametersValueType, unsigned int NDimensions>
TimeVaryingBSplineVelocityFieldTransform<TParametersValueType, NDimensions>
::TimeVaryingBSplineVelocityFieldTransform()
{
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
template<typename TParametersValueType, unsigned int NDimensions>
TimeVaryingBSplineVelocityFieldTransform<TParametersValueType, NDimensions>::
~TimeVaryingBSplineVelocityFieldTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TParametersValueType, NDimensions>
::IntegrateVelocityField()
{
  if( !this->GetVelocityField() )
    {
    itkExceptionMacro( "The B-spline velocity field does not exist." );
    }

  typedef BSplineControlPointImageFilter<VelocityFieldType, VelocityFieldType> BSplineFilterType;

  typename BSplineFilterType::ArrayType closeDimensions;
  closeDimensions.Fill( 0 );
  if( this->m_TemporalPeriodicity )
    {
    closeDimensions[NDimensions] = 1;
    }

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetInput( this->GetTimeVaryingVelocityFieldControlPointLattice() );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetSpacing( this->m_VelocityFieldSpacing );
  bspliner->SetSize( this->m_VelocityFieldSize );
  bspliner->SetDirection( this->m_VelocityFieldDirection );
  bspliner->SetOrigin( this->m_VelocityFieldOrigin );
  bspliner->SetCloseDimension( closeDimensions );
  bspliner->Update();

  typename VelocityFieldType::Pointer bsplinerOutput = bspliner->GetOutput();
  bsplinerOutput->DisconnectPipeline();

  typedef TimeVaryingVelocityFieldIntegrationImageFilter<VelocityFieldType, DisplacementFieldType> IntegratorType;

  typename IntegratorType::Pointer integrator = IntegratorType::New();
  integrator->SetInput( bsplinerOutput );
  integrator->SetLowerTimeBound( this->GetLowerTimeBound() );
  integrator->SetUpperTimeBound( this->GetUpperTimeBound() );

  if( this->GetVelocityFieldInterpolator() )
    {
    integrator->SetVelocityFieldInterpolator( this->GetModifiableVelocityFieldInterpolator() );
    }

  integrator->SetNumberOfIntegrationSteps( this->GetNumberOfIntegrationSteps() );
  integrator->Update();

  typename DisplacementFieldType::Pointer displacementField = integrator->GetOutput();
  displacementField->DisconnectPipeline();

  this->SetDisplacementField( displacementField );
  this->GetModifiableInterpolator()->SetInputImage( displacementField );

  typename IntegratorType::Pointer inverseIntegrator = IntegratorType::New();
  inverseIntegrator->SetInput( bsplinerOutput );
  inverseIntegrator->SetLowerTimeBound( this->GetUpperTimeBound() );
  inverseIntegrator->SetUpperTimeBound( this->GetLowerTimeBound() );

  if( this->GetVelocityFieldInterpolator() )
    {
    inverseIntegrator->SetVelocityFieldInterpolator( this->GetModifiableVelocityFieldInterpolator() );
    }

  inverseIntegrator->SetNumberOfIntegrationSteps( this->GetNumberOfIntegrationSteps() );
  inverseIntegrator->Update();

  typename DisplacementFieldType::Pointer inverseDisplacementField = inverseIntegrator->GetOutput();
  inverseDisplacementField->DisconnectPipeline();

  this->SetInverseDisplacementField( inverseDisplacementField );
}

template<typename TParametersValueType, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
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
  importer->SetRegion( this->GetTimeVaryingVelocityFieldControlPointLattice()->GetBufferedRegion() );
  importer->SetOrigin( this->GetTimeVaryingVelocityFieldControlPointLattice()->GetOrigin() );
  importer->SetSpacing( this->GetTimeVaryingVelocityFieldControlPointLattice()->GetSpacing() );
  importer->SetDirection( this->GetTimeVaryingVelocityFieldControlPointLattice()->GetDirection() );
  importer->Update();

  typedef AddImageFilter<VelocityFieldType, VelocityFieldType, VelocityFieldType> AdderType;
  typename AdderType::Pointer adder = AdderType::New();
  adder->SetInput1( this->GetVelocityField() );
  adder->SetInput2( importer->GetOutput() );

  typename VelocityFieldType::Pointer totalFieldLattice = adder->GetOutput();
  totalFieldLattice->Update();

  this->SetTimeVaryingVelocityFieldControlPointLattice( totalFieldLattice );
  this->IntegrateVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
void
TimeVaryingBSplineVelocityFieldTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;

  os << indent << "Sampled velocity field parameters" << std::endl;
  os << indent << "  size: " << this->m_VelocityFieldSize << std::endl;
  os << indent << "  spacing: " << this->m_VelocityFieldSpacing << std::endl;
  os << indent << "  origin: " << this->m_VelocityFieldOrigin << std::endl;
  os << indent << "  direction: " << this->m_VelocityFieldDirection << std::endl;
}
} // namespace itk

#endif
