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
#ifndef itkConstantVelocityFieldTransform_hxx
#define itkConstantVelocityFieldTransform_hxx

#include "itkConstantVelocityFieldTransform.h"

#include "itkExponentialDisplacementFieldImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::ConstantVelocityFieldTransform() :
  m_ConstantVelocityField( ITK_NULLPTR ),
  m_CalculateNumberOfIntegrationStepsAutomatically( false ),
  m_ConstantVelocityFieldSetTime( 0 )
{
  this->m_FixedParameters.SetSize( ConstantVelocityFieldDimension * ( ConstantVelocityFieldDimension + 3 ) );
  this->m_FixedParameters.Fill( 0.0 );

  this->m_LowerTimeBound = 0.0;
  this->m_UpperTimeBound = 1.0;

  this->m_NumberOfIntegrationSteps = 10;

  // Setup and assign default interpolator
  typedef VectorLinearInterpolateImageFunction<ConstantVelocityFieldType, ScalarType> DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer interpolator = DefaultInterpolatorType::New();
  this->m_ConstantVelocityFieldInterpolator = interpolator;

  // Setup and assign parameter helper. This will hold the displacement field
  // for access through the common OptimizerParameters interface.
  OptimizerParametersHelperType* helper = new OptimizerParametersHelperType;
  // After assigning this, this->m_Parameter will manage this,
  // deleting when appropriate.
  this->m_Parameters.SetHelper( helper );
}

/**
 * Destructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>::
~ConstantVelocityFieldTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor)
{
  // This simply adds the values.
  // TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters( update, factor );

  this->IntegrateVelocityField();
}

/**
 * return an inverse transformation
 */
template<typename TParametersValueType, unsigned int NDimensions>
bool
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_ConstantVelocityField )
    {
    return false;
    }
  else
    {
    inverse->SetFixedParameters(this->GetFixedParameters());
    inverse->SetUpperTimeBound( this->GetLowerTimeBound() );
    inverse->SetLowerTimeBound( this->GetUpperTimeBound() );
    inverse->SetDisplacementField( this->m_InverseDisplacementField );
    inverse->SetInverseDisplacementField( this->m_DisplacementField );
    inverse->SetInterpolator( this->m_Interpolator );
    inverse->SetConstantVelocityField( this->m_ConstantVelocityField );
    inverse->SetConstantVelocityFieldInterpolator( this->m_ConstantVelocityFieldInterpolator );
    return true;
    }
}

// Return an inverse of this transform
template<typename TParametersValueType, unsigned int NDimensions>
typename ConstantVelocityFieldTransform<TParametersValueType, NDimensions>::InverseTransformBasePointer
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::GetInverseTransform() const
{
  Pointer inverseTransform = New();
  if( this->GetInverse( inverseTransform ) )
    {
    return inverseTransform.GetPointer();
    }
  else
    {
    return ITK_NULLPTR;
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
void ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::SetConstantVelocityField( ConstantVelocityFieldType* field )
{
  itkDebugMacro( "setting VelocityField to " << field );
  if( this->m_ConstantVelocityField != field )
    {
    this->m_ConstantVelocityField = field;

    this->Modified();
    /* Store this separately for use in smoothing because we only want
     * to know when the displacement field object has changed, not just
     * its contents. */
    this->m_ConstantVelocityFieldSetTime = this->GetMTime();
    if( !this->m_ConstantVelocityFieldInterpolator.IsNull() )
      {
      this->m_ConstantVelocityFieldInterpolator->SetInputImage( this->m_ConstantVelocityField );
      }
    // Assign to parameters object
    this->m_Parameters.SetParametersObject( this->m_ConstantVelocityField );
    }
  this->SetFixedParametersFromConstantVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::SetConstantVelocityFieldInterpolator( ConstantVelocityFieldInterpolatorType* interpolator )
{
  itkDebugMacro( "setting ConstantVelocityFieldInterpolator to " << interpolator );
  if( this->m_ConstantVelocityFieldInterpolator != interpolator )
    {
    this->m_ConstantVelocityFieldInterpolator = interpolator;
    this->Modified();
    if( !this->m_ConstantVelocityField.IsNull() )
      {
      this->m_ConstantVelocityFieldInterpolator->SetInputImage( this->m_ConstantVelocityField );
      }
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::SetFixedParameters( const FixedParametersType & fixedParameters )
{
  if( fixedParameters.Size() != ConstantVelocityFieldDimension * ( ConstantVelocityFieldDimension + 3 ) )
    {
    itkExceptionMacro( "The fixed parameters are not the right size." );
    }

  SizeType size;
  for( unsigned int d = 0; d < ConstantVelocityFieldDimension; d++ )
    {
    size[d] = static_cast<SizeValueType>( fixedParameters[d] );
    }

  PointType origin;
  for( unsigned int d = 0; d < ConstantVelocityFieldDimension; d++ )
    {
    origin[d] = fixedParameters[d + ConstantVelocityFieldDimension];
    }

  SpacingType spacing;
  for( unsigned int d = 0; d < ConstantVelocityFieldDimension; d++ )
    {
    spacing[d] = fixedParameters[d + 2 * ConstantVelocityFieldDimension];
    }

  DirectionType direction;
  for( unsigned int di = 0; di < ConstantVelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < ConstantVelocityFieldDimension; dj++ )
      {
      direction[di][dj] = fixedParameters[3 * ConstantVelocityFieldDimension + ( di * ConstantVelocityFieldDimension + dj )];
      }
    }

  PixelType zeroDisplacement;
  zeroDisplacement.Fill( 0.0 );

  typename ConstantVelocityFieldType::Pointer velocityField = ConstantVelocityFieldType::New();
  velocityField->SetSpacing( spacing );
  velocityField->SetOrigin( origin );
  velocityField->SetDirection( direction );
  velocityField->SetRegions( size );
  velocityField->Allocate();
  velocityField->FillBuffer( zeroDisplacement );

  this->SetConstantVelocityField( velocityField );
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::SetFixedParametersFromConstantVelocityField() const
  {
  this->m_FixedParameters.SetSize( ConstantVelocityFieldDimension * ( ConstantVelocityFieldDimension + 3 ) );

  const typename ConstantVelocityFieldType::RegionType & fieldRegion =
    this->m_ConstantVelocityField->GetLargestPossibleRegion();

  // Set the field size parameters
  SizeType fieldSize = fieldRegion.GetSize();
  for( unsigned int i = 0; i < ConstantVelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>( fieldSize[i] );
    }

  // Set the origin parameters
  PointType fieldOrigin = this->m_ConstantVelocityField->GetOrigin();
  for( unsigned int i = 0; i < ConstantVelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[ConstantVelocityFieldDimension + i] = fieldOrigin[i];
    }

  // Set the spacing parameters
  SpacingType fieldSpacing = this->m_ConstantVelocityField->GetSpacing();
  for( unsigned int i = 0; i < ConstantVelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[2 * ConstantVelocityFieldDimension + i] = static_cast<FixedParametersValueType>( fieldSpacing[i] );
    }

  // Set the direction parameters
  DirectionType fieldDirection = this->m_ConstantVelocityField->GetDirection();
  for( unsigned int di = 0; di < ConstantVelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < ConstantVelocityFieldDimension; dj++ )
      {
      this->m_FixedParameters[3 * ConstantVelocityFieldDimension + ( di * ConstantVelocityFieldDimension + dj )] =
        static_cast<FixedParametersValueType>( fieldDirection[di][dj] );
      }
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::IntegrateVelocityField()
{
  typedef ExponentialDisplacementFieldImageFilter<ConstantVelocityFieldType, ConstantVelocityFieldType>
    ExponentiatorType;

  ConstantVelocityFieldPointer constantVelocityField = this->GetModifiableConstantVelocityField();

  typename ExponentiatorType::Pointer exponentiator = ExponentiatorType::New();
  exponentiator->SetInput( constantVelocityField );
  if( this->m_CalculateNumberOfIntegrationStepsAutomatically || this->GetNumberOfIntegrationSteps() == 0 )
    {
    exponentiator->SetAutomaticNumberOfIterations( true );
    if( !this->m_CalculateNumberOfIntegrationStepsAutomatically && this->m_NumberOfIntegrationSteps == 0 )
      {
      itkWarningMacro( "Number of integration steps is 0.  Calculating the number of integration steps automatically." );
      }
    }
  else
    {
    exponentiator->SetAutomaticNumberOfIterations( false );
    exponentiator->SetMaximumNumberOfIterations( this->GetNumberOfIntegrationSteps() );
    }
  exponentiator->SetComputeInverse( false );
  exponentiator->Update();

  // Calculate inverse displacement field

  typename ExponentiatorType::Pointer exponentiatorInv = ExponentiatorType::New();
  exponentiatorInv->SetInput( constantVelocityField );
  if( this->m_CalculateNumberOfIntegrationStepsAutomatically || this->m_NumberOfIntegrationSteps == 0 )
    {
    exponentiatorInv->SetAutomaticNumberOfIterations( true );
    if( !this->m_CalculateNumberOfIntegrationStepsAutomatically && this->m_NumberOfIntegrationSteps == 0 )
      {
      itkWarningMacro( "Number of integration steps is 0.  Calculating the number of integration steps automatically." );
      }
    }
  else
    {
    exponentiatorInv->SetAutomaticNumberOfIterations( false );
    exponentiatorInv->SetMaximumNumberOfIterations( this->GetNumberOfIntegrationSteps() );
    }
  exponentiatorInv->SetComputeInverse( true );
  exponentiatorInv->Update();

  // We use the lower and upper time bounds to keep track of which results should go in
  // the forward and inverse displacement fields.  This is useful when calling and tracking
  // the inverse transform where the velocity field is the same for both the forward and
  // inverse transforms but the upper and lower time bounds are switched as well as the
  // forward and inverse displacement fields.

  if( this->GetLowerTimeBound() <= this->GetUpperTimeBound() )
    {
    this->SetDisplacementField( exponentiator->GetOutput() );
    this->SetInverseDisplacementField( exponentiatorInv->GetOutput() );
    }
  else
    {
    this->SetDisplacementField( exponentiatorInv->GetOutput() );
    this->SetInverseDisplacementField( exponentiator->GetOutput() );
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
typename ConstantVelocityFieldTransform<TParametersValueType, NDimensions>::DisplacementFieldType::Pointer
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::CopyDisplacementField( const DisplacementFieldType *toCopy ) const
{
  typename DisplacementFieldType::Pointer rval = DisplacementFieldType::New();
  rval->SetOrigin( toCopy->GetOrigin() );
  rval->SetSpacing( toCopy->GetSpacing() );
  rval->SetDirection( toCopy->GetDirection() );
  rval->SetRegions( toCopy->GetLargestPossibleRegion() );
  rval->Allocate();

  ImageRegionConstIterator<DisplacementFieldType> dispIt( toCopy,toCopy->GetLargestPossibleRegion() );
  ImageRegionIterator<DisplacementFieldType> cloneDispIt( rval,rval->GetLargestPossibleRegion() );
  for( dispIt.GoToBegin(), cloneDispIt.GoToBegin(); !dispIt.IsAtEnd() && !cloneDispIt.IsAtEnd();
      ++dispIt, ++cloneDispIt )
    {
    cloneDispIt.Set( dispIt.Get() );
    }
  return rval;
}

template<typename TParametersValueType, unsigned int NDimensions>
typename LightObject::Pointer
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::InternalClone() const
{
  // create a new instance
  LightObject::Pointer loPtr = Superclass::InternalClone();
  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  // set the fixed/moving parameters.
  // Not sure these do anything at all useful!
  rval->SetFixedParameters( this->GetFixedParameters() );
  rval->SetParameters( this->GetParameters() );

  // need the displacement field but GetDisplacementField is non-const.
  Self *nonConstThis = const_cast<Self *>(this);
  typename DisplacementFieldType::ConstPointer dispField = nonConstThis->GetDisplacementField();
  typename DisplacementFieldType::Pointer cloneDispField =
    this->CopyDisplacementField(dispField.GetPointer());
  rval->GetModifiableInterpolator()->SetInputImage( cloneDispField );
  rval->SetDisplacementField( cloneDispField );

  // now do the inverse -- it actually gets created as a side effect?
  typename DisplacementFieldType::ConstPointer invDispField = nonConstThis->GetInverseDisplacementField();
  typename DisplacementFieldType::Pointer cloneInvDispField = this->CopyDisplacementField( invDispField.GetPointer() );
  rval->SetInverseDisplacementField( cloneInvDispField );

  // copy the VelocityField
  // SetFixedParameters allocates the VelocityField
  ImageRegionConstIterator<ConstantVelocityFieldType>
    thisIt( this->m_ConstantVelocityField, this->m_ConstantVelocityField->GetLargestPossibleRegion() );
  ImageRegionIterator<ConstantVelocityFieldType> cloneIt( rval->m_ConstantVelocityField,
    rval->m_ConstantVelocityField->GetLargestPossibleRegion() );
  for( thisIt.GoToBegin(),cloneIt.GoToBegin(); !thisIt.IsAtEnd() && !cloneIt.IsAtEnd();
      ++thisIt, ++cloneIt )
    {
    cloneIt.Set( thisIt.Get() );
    }

  // set config parameters
  rval->SetLowerTimeBound( this->GetLowerTimeBound() );
  rval->SetUpperTimeBound( this->GetUpperTimeBound() );
  rval->SetNumberOfIntegrationSteps( this->GetNumberOfIntegrationSteps() );

  // copy the interpolator
  ConstantVelocityFieldInterpolatorPointer newInterp = dynamic_cast<ConstantVelocityFieldInterpolatorType *>
    ( this->m_ConstantVelocityFieldInterpolator->CreateAnother().GetPointer() );
  // interpolator needs to know about the velocity field
  newInterp->SetInputImage( rval->GetConstantVelocityField() );
  rval->SetConstantVelocityFieldInterpolator( newInterp );
  return loPtr;
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  itkPrintSelfObjectMacro( ConstantVelocityFieldInterpolator );
  itkPrintSelfObjectMacro( ConstantVelocityField );

  os << indent << "LowerTimeBound: " << this->m_LowerTimeBound << std::endl;
  os << indent << "UpperTimeBound: " << this->m_UpperTimeBound << std::endl;
  os << indent << "NumberOfIntegrationSteps: "
    << this->m_NumberOfIntegrationSteps << std::endl;
}

} // namespace itk

#endif
