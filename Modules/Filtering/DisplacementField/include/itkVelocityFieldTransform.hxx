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
#ifndef itkVelocityFieldTransform_hxx
#define itkVelocityFieldTransform_hxx

#include "itkVelocityFieldTransform.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
VelocityFieldTransform<TParametersValueType, NDimensions>
::VelocityFieldTransform()
{
  this->m_FixedParameters.SetSize( VelocityFieldDimension * ( VelocityFieldDimension + 3 ) );
  this->m_FixedParameters.Fill( 0.0 );

  this->m_LowerTimeBound = 0.0;
  this->m_UpperTimeBound = 1.0;

  this->m_NumberOfIntegrationSteps = 10;

  // Setup and assign default interpolator
  typedef VectorLinearInterpolateImageFunction<VelocityFieldType, ScalarType> DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer interpolator = DefaultInterpolatorType::New();
  this->m_VelocityFieldInterpolator = interpolator;

  // Setup and assign parameter helper. This will hold the displacement field
  // for access through the common OptimizerParameters interface.
  OptimizerParametersHelperType* helper = new OptimizerParametersHelperType;
  // After assigning this, this->m_Parameter will manage this,
  // deleting when appropriate.
  this->m_Parameters.SetHelper( helper );

  this->m_VelocityFieldSetTime = 0;
}

/**
 * Destructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
VelocityFieldTransform<TParametersValueType, NDimensions>::
~VelocityFieldTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
VelocityFieldTransform<TParametersValueType, NDimensions>
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
VelocityFieldTransform<TParametersValueType, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_VelocityField )
    {
    return false;
    }
  else
    {
    inverse->SetFixedParameters(this->GetFixedParameters());
    inverse->SetUpperTimeBound( this->m_LowerTimeBound );
    inverse->SetLowerTimeBound( this->m_UpperTimeBound );
    inverse->SetDisplacementField( this->m_InverseDisplacementField );
    inverse->SetInverseDisplacementField( this->m_DisplacementField );
    inverse->SetInterpolator( this->m_Interpolator );
    inverse->SetVelocityField( this->m_VelocityField );
    inverse->SetVelocityFieldInterpolator( this->m_VelocityFieldInterpolator );
    return true;
    }
}

// Return an inverse of this transform
template<typename TParametersValueType, unsigned int NDimensions>
typename VelocityFieldTransform<TParametersValueType, NDimensions>::InverseTransformBasePointer
VelocityFieldTransform<TParametersValueType, NDimensions>
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
void VelocityFieldTransform<TParametersValueType, NDimensions>
::SetVelocityField( VelocityFieldType* field )
{
  itkDebugMacro( "setting VelocityField to " << field );
  if( this->m_VelocityField != field )
    {
    this->m_VelocityField = field;

    this->Modified();
    /* Store this separately for use in smoothing because we only want
     * to know when the displacement field object has changed, not just
     * its contents. */
    this->m_VelocityFieldSetTime = this->GetMTime();
    if( !this->m_VelocityFieldInterpolator.IsNull() )
      {
      this->m_VelocityFieldInterpolator->SetInputImage( this->m_VelocityField );
      }
    // Assign to parameters object
    this->m_Parameters.SetParametersObject( this->m_VelocityField );
    }
  this->SetFixedParametersFromVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
void
VelocityFieldTransform<TParametersValueType, NDimensions>
::SetVelocityFieldInterpolator( VelocityFieldInterpolatorType* interpolator )
{
  itkDebugMacro( "setting VelocityFieldInterpolator to " << interpolator );
  if( this->m_VelocityFieldInterpolator != interpolator )
    {
    this->m_VelocityFieldInterpolator = interpolator;
    this->Modified();
    if( !this->m_VelocityField.IsNull() )
      {
      this->m_VelocityFieldInterpolator->SetInputImage( this->m_VelocityField );
      }
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
void
VelocityFieldTransform<TParametersValueType, NDimensions>
::SetFixedParameters( const FixedParametersType & fixedParameters )
{
  if( fixedParameters.Size() != VelocityFieldDimension * ( VelocityFieldDimension + 3 ) )
    {
    itkExceptionMacro( "The fixed parameters are not the right size." );
    }

  SizeType size;
  for( unsigned int d = 0; d < VelocityFieldDimension; d++ )
    {
    size[d] = static_cast<SizeValueType>( fixedParameters[d] );
    }

  PointType origin;
  for( unsigned int d = 0; d < VelocityFieldDimension; d++ )
    {
    origin[d] = fixedParameters[d + VelocityFieldDimension];
    }

  SpacingType spacing;
  for( unsigned int d = 0; d < VelocityFieldDimension; d++ )
    {
    spacing[d] = fixedParameters[d + 2 * VelocityFieldDimension];
    }

  DirectionType direction;
  for( unsigned int di = 0; di < VelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < VelocityFieldDimension; dj++ )
      {
      direction[di][dj] = fixedParameters[3 * VelocityFieldDimension + ( di * VelocityFieldDimension + dj )];
      }
    }

  PixelType zeroDisplacement;
  zeroDisplacement.Fill( 0.0 );

  typename VelocityFieldType::Pointer velocityField = VelocityFieldType::New();
  velocityField->SetSpacing( spacing );
  velocityField->SetOrigin( origin );
  velocityField->SetDirection( direction );
  velocityField->SetRegions( size );
  velocityField->Allocate();
  velocityField->FillBuffer( zeroDisplacement );

  this->SetVelocityField( velocityField );
}

template<typename TParametersValueType, unsigned int NDimensions>
void
VelocityFieldTransform<TParametersValueType, NDimensions>
::SetFixedParametersFromVelocityField() const
  {
  this->m_FixedParameters.SetSize( VelocityFieldDimension * ( VelocityFieldDimension + 3 ) );

  const typename VelocityFieldType::RegionType & fieldRegion =
    this->m_VelocityField->GetLargestPossibleRegion();

  // Set the field size parameters
  SizeType fieldSize = fieldRegion.GetSize();
  for( unsigned int i = 0; i < VelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>( fieldSize[i] );
    }

  // Set the origin parameters
  PointType fieldOrigin = this->m_VelocityField->GetOrigin();
  for( unsigned int i = 0; i < VelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[VelocityFieldDimension + i] = fieldOrigin[i];
    }

  // Set the spacing parameters
  SpacingType fieldSpacing = this->m_VelocityField->GetSpacing();
  for( unsigned int i = 0; i < VelocityFieldDimension; i++ )
    {
    this->m_FixedParameters[2 * VelocityFieldDimension + i] = static_cast<FixedParametersValueType>( fieldSpacing[i] );
    }

  // Set the direction parameters
  DirectionType fieldDirection = this->m_VelocityField->GetDirection();
  for( unsigned int di = 0; di < VelocityFieldDimension; di++ )
    {
    for( unsigned int dj = 0; dj < VelocityFieldDimension; dj++ )
      {
      this->m_FixedParameters[3 * VelocityFieldDimension + ( di * VelocityFieldDimension + dj )] =
        static_cast<FixedParametersValueType>( fieldDirection[di][dj] );
      }
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
typename VelocityFieldTransform<TParametersValueType, NDimensions>::DisplacementFieldType::Pointer
VelocityFieldTransform<TParametersValueType, NDimensions>
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
VelocityFieldTransform<TParametersValueType, NDimensions>
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
  ImageRegionConstIterator<VelocityFieldType>
    thisIt( this->m_VelocityField, this->m_VelocityField->GetLargestPossibleRegion() );
  ImageRegionIterator<VelocityFieldType> cloneIt( rval->m_VelocityField,
    rval->m_VelocityField->GetLargestPossibleRegion() );
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
  VelocityFieldInterpolatorPointer newInterp = dynamic_cast<VelocityFieldInterpolatorType *>
    ( this->m_VelocityFieldInterpolator->CreateAnother().GetPointer() );
  if(newInterp.IsNull())
    {
    itkExceptionMacro(<< "dynamic_cast failed.");
    }

  // interpolator needs to know about the velocity field
  newInterp->SetInputImage( rval->GetVelocityField() );
  rval->SetVelocityFieldInterpolator( newInterp );
  return loPtr;
}

template<typename TParametersValueType, unsigned int NDimensions>
void
VelocityFieldTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Interpolator: " << std::endl;
  os << indent << indent << this->m_VelocityFieldInterpolator << std::endl;

  if( this->m_VelocityField )
    {
    os << indent << "Velocity Field: " << std::endl;
    os << indent << indent << this->m_VelocityField << std::endl;
    }

  os << indent << "LowerTimeBound: " << this->m_LowerTimeBound << std::endl;
  os << indent << "UpperTimeBound: " << this->m_UpperTimeBound << std::endl;
  os << indent << "NumberOfIntegrationSteps: "
    << this->m_NumberOfIntegrationSteps << std::endl;
}

} // namespace itk

#endif
