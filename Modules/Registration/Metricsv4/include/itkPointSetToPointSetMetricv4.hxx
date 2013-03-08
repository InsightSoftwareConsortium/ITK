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
#ifndef __itkPointSetToPointSetMetricv4_hxx
#define __itkPointSetToPointSetMetricv4_hxx

#include "itkPointSetToPointSetMetricv4.h"
#include "itkIdentityTransform.h"

namespace itk
{

/** Constructor */
template<class TFixedPointSet, class TMovingPointSet>
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::PointSetToPointSetMetricv4()
{
  this->m_FixedPointSet = NULL;    // has to be provided by the user.
  this->m_MovingPointSet = NULL;    // has to be provided by the user.

  this->m_FixedTransformedPointSet = NULL;
  this->m_MovingTransformedPointSet = NULL;
  this->m_VirtualTransformedPointSet = NULL;

  this->m_FixedTransformedPointsLocator = NULL;
  this->m_MovingTransformedPointsLocator = NULL;

  this->m_MovingTransformPointLocatorsNeedInitialization = false;
  this->m_FixedTransformPointLocatorsNeedInitialization = false;

  this->m_MovingTransformedPointSetTime = this->GetMTime();
  this->m_FixedTransformedPointSetTime = this->GetMTime();

  // We iterate over the fixed points to calculate the value and derivative.
  this->SetGradientSource( Superclass::GRADIENT_SOURCE_FIXED );

  this->m_HaveWarnedAboutNumberOfValidPoints = false;

  this->m_UsePointSetData = false;
}

/** Destructor */
template<class TFixedPointSet, class TMovingPointSet>
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::~PointSetToPointSetMetricv4()
{
}

/** Initialize the metric */
template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::Initialize( void ) throw ( ExceptionObject )
{
  if ( !this->m_FixedPointSet )
    {
    itkExceptionMacro( "Fixed point set is not present" );
    }

  if ( !this->m_MovingPointSet )
    {
    itkExceptionMacro( "Moving point set is not present" );
    }

  // We don't know how to support gradient source of type moving
  if( this->GetGradientSourceIncludesMoving() )
    {
    itkExceptionMacro("GradientSource includes GRADIENT_SOURCE_MOVING. Not supported.");
    }

  // If the PointSet is provided by a source, update the source.
  if( this->m_MovingPointSet->GetSource() )
    {
    this->m_MovingPointSet->GetSource()->Update();
    }

  // If the point set is provided by a source, update the source.
  if( this->m_FixedPointSet->GetSource() )
    {
    this->m_FixedPointSet->GetSource()->Update();
    }

  // Check for virtual domain if needed.
  // With local-support transforms we need a virtual domain in
  // order to properly store the per-point derivatives.
  // This will create a virtual domain that matches the DisplacementFieldTransform.
  // If the virutal domain has already been set, it will
  // be verified against the transform in Superclass::Initialize.
  if( this->HasLocalSupport() )
    {
    if( ! this->m_UserHasSetVirtualDomain )
      {
      typename DisplacementFieldTransformType::Pointer displacementTransform;
      displacementTransform = this->GetMovingDisplacementFieldTransform();
      if( displacementTransform.IsNull() )
        {
        itkExceptionMacro("Expected the moving transform to be of type DisplacementFieldTransform or derived, "
                          "or a CompositeTransform with DisplacementFieldTransform as the last to have been added." );
        }
      typedef typename DisplacementFieldTransformType::DisplacementFieldType DisplacementFieldType;
      typename DisplacementFieldType::ConstPointer field = displacementTransform->GetDisplacementField();
      this->SetVirtualDomain( field->GetSpacing(), field->GetOrigin(), field->GetDirection(), field->GetBufferedRegion() );
      }
    }

  // Superclass initialization. Do after checking for virtual domain.
  Superclass::Initialize();

  // Call this now for derived classes that need
  // a member to be initialized during Initialize().
  this->InitializePointSets();
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::InitializePointSets() const
{
  this->TransformMovingPointSet();
  this->TransformFixedAndCreateVirtualPointSet();
  this->InitializePointsLocators();
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::InitializeForIteration() const
{
  this->InitializePointSets();
  this->m_NumberOfValidPoints = this->CalculateNumberOfValidFixedPoints();
  if( this->m_NumberOfValidPoints < this->GetNumberOfComponents() && ! this->m_HaveWarnedAboutNumberOfValidPoints)
    {
    itkWarningMacro( "Only " << this->m_NumberOfValidPoints << " of " << this->GetNumberOfComponents() << " points are within the virtual domain, and will be used in the evaluation." );
    this->m_HaveWarnedAboutNumberOfValidPoints = true;
    }
}

template<class TFixedPointSet, class TMovingPointSet>
SizeValueType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetNumberOfComponents() const
{
  return this->m_FixedTransformedPointSet->GetNumberOfPoints();
}

template<class TFixedPointSet, class TMovingPointSet>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>::MeasureType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetValue() const
{
  this->InitializeForIteration();

  MeasureType value = 0.0;

  PointsConstIterator It = this->m_FixedTransformedPointSet->GetPoints()->Begin();
  // Virtual point set will be the same size as fixed point set as long as it's
  // generated from the fixed point set.
  if( this->m_VirtualTransformedPointSet->GetNumberOfPoints() != this->m_FixedTransformedPointSet->GetNumberOfPoints() )
    {
    itkExceptionMacro("Expected FixedTransformedPointSet to be the same size as VirtualTransformedPointSet.");
    }
  PointsConstIterator virtualIt = this->m_VirtualTransformedPointSet->GetPoints()->Begin();

  while( It != this->m_FixedTransformedPointSet->GetPoints()->End() )
    {
    /* Verify the virtual point is in the virtual domain.
     * If user hasn't defined a virtual space, and the active transform is not
     * a displacement field transform type, then this will always return true. */
    if( ! this->IsInsideVirtualDomain( virtualIt.Value() ) )
      {
      ++It;
      ++virtualIt;
      continue;
      }

    PixelType pixel = 0;
    if( this->m_UsePointSetData )
      {
      this->m_FixedPointSet->GetPointData( It.Index(), &pixel );
      }

    value += this->GetLocalNeighborhoodValue( It.Value(), pixel );
    ++virtualIt;
    ++It;
    }

  DerivativeType derivative;
  if( this->VerifyNumberOfValidPoints( value, derivative ) )
    {
    value /= static_cast<MeasureType>( this->m_NumberOfValidPoints );
    }
  this->m_Value = value;

  return value;
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetDerivative( DerivativeType & derivative ) const
{
  MeasureType value = NumericTraits<MeasureType>::Zero;
  this->CalculateValueAndDerivative( value, derivative, false );
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const
{
  this->CalculateValueAndDerivative( value, derivative, true );
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::CalculateValueAndDerivative( MeasureType & value, DerivativeType & derivative, bool calculateValue ) const
{
  this->InitializeForIteration();
  derivative.SetSize( this->GetNumberOfParameters() );
  derivative.Fill( NumericTraits<DerivativeValueType>::Zero );

  value = NumericTraits<MeasureType>::Zero;
  MovingTransformJacobianType  jacobian( MovingPointDimension, this->GetNumberOfLocalParameters() );

  DerivativeType localTransformDerivative( this->GetNumberOfLocalParameters() );
  localTransformDerivative.Fill( NumericTraits<DerivativeValueType>::Zero );

  // Virtual point set will be the same size as fixed point set as long as it's
  // generated from the fixed point set.
  if( this->m_VirtualTransformedPointSet->GetNumberOfPoints() != this->m_FixedTransformedPointSet->GetNumberOfPoints() )
    {
    itkExceptionMacro("Expected FixedTransformedPointSet to be the same size as VirtualTransformedPointSet.");
    }
  PointsConstIterator virtualIt = this->m_VirtualTransformedPointSet->GetPoints()->Begin();
  PointsConstIterator It = this->m_FixedTransformedPointSet->GetPoints()->Begin();
  PointsConstIterator end = this->m_FixedTransformedPointSet->GetPoints()->End();

  while( It != end )
    {
    MeasureType pointValue = NumericTraits<MeasureType>::Zero;
    LocalDerivativeType pointDerivative;

    /* Verify the virtual point is in the virtual domain.
     * If user hasn't defined a virtual space, and the active transform is not
     * a displacement field transform type, then this will always return true. */
    if( ! this->IsInsideVirtualDomain( virtualIt.Value() ) )
      {
      ++It;
      ++virtualIt;
      continue;
      }

    PixelType pixel = 0;
    if( this->m_UsePointSetData )
      {
      this->m_FixedPointSet->GetPointData( It.Index(), &pixel );
      }

    if( calculateValue )
      {
      this->GetLocalNeighborhoodValueAndDerivative( It.Value(), pointValue, pointDerivative, pixel );
      value += pointValue;
      }
    else
      {
      pointDerivative = this->GetLocalNeighborhoodDerivative( It.Value(), pixel );
      }

    // Map into parameter space
    if( this->HasLocalSupport() )
      {
      // Reset to zero since we're not accumulating in the local-support case.
      localTransformDerivative.Fill( NumericTraits<DerivativeValueType>::Zero );
      }
    this->GetMovingTransform()->ComputeJacobianWithRespectToParameters( virtualIt.Value(), jacobian );
    for ( NumberOfParametersType par = 0; par < this->GetNumberOfLocalParameters(); par++ )
      {
      for( DimensionType d = 0; d < PointDimension; ++d )
        {
        localTransformDerivative[par] += jacobian(d, par) * pointDerivative[d];
        }
      }

    // For local-support transforms, store the per-point result
    if( this->HasLocalSupport() )
      {
      this->StorePointDerivative( virtualIt.Value(), localTransformDerivative, derivative );
      }

    ++It;
    ++virtualIt;
    }

  if( this->VerifyNumberOfValidPoints( value, derivative ) )
    {
    // For global-support transforms, average the accumulated derivative result
    if( ! this->HasLocalSupport() )
      {
      derivative = localTransformDerivative / static_cast<DerivativeValueType>(this->m_NumberOfValidPoints);
      }
    value /= static_cast<MeasureType>( this->m_NumberOfValidPoints );
    }
  this->m_Value = value;
}

template<class TFixedPointSet, class TMovingPointSet>
SizeValueType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::CalculateNumberOfValidFixedPoints() const
{
  // Determine the number of valid fixed points, using
  // their positions in the virtual domain.
  SizeValueType numberOfValidPoints = NumericTraits<SizeValueType>::Zero;
  PointsConstIterator virtualIt = this->m_VirtualTransformedPointSet->GetPoints()->Begin();
  while( virtualIt != this->m_VirtualTransformedPointSet->GetPoints()->End() )
    {
    if( this->IsInsideVirtualDomain( virtualIt.Value() ) )
      {
      ++numberOfValidPoints;
      }
    ++virtualIt;
    }
  return numberOfValidPoints;
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::StorePointDerivative( const VirtualPointType & virtualPoint, const DerivativeType & pointDerivative, DerivativeType & field ) const
{
  // Update derivative field at some index.
  // This requires the active transform displacement field to be the
  // same size as virtual domain, and that VirtualImage PixelType
  // is scalar (both of which are verified during Metric initialization).
  try
    {
    OffsetValueType offset = this->ComputeParameterOffsetFromVirtualPoint( virtualPoint, this->GetNumberOfLocalParameters() );
    for (NumberOfParametersType i=0; i < this->GetNumberOfLocalParameters(); i++)
      {
      /* Be sure to *add* here and not assign. Required for proper behavior
       * with multi-variate metric. */
      field[offset+i] += pointDerivative[i];
      }
    }
  catch( ExceptionObject & exc )
    {
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }
}

template<class TFixedPointSet, class TMovingPointSet>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::LocalDerivativeType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetLocalNeighborhoodDerivative( const PointType & point, const PixelType & pixel ) const
{
  MeasureType measure;
  LocalDerivativeType localDerivative;
  this->GetLocalNeighborhoodValueAndDerivative( point, measure, localDerivative, pixel );
  return localDerivative;
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::TransformMovingPointSet() const
{
  // Transform the moving point set with the moving transform.
  // We calculate the value and derivatives in the moving space.
  if( ( this->GetMTime() > this->m_MovingTransformedPointSetTime ) || ( this->m_MovingTransform->GetMTime() > this->GetMTime() ) || !this->m_MovingTransformedPointSet )
    {
    this->m_MovingTransformPointLocatorsNeedInitialization = true;
    this->m_MovingTransformedPointSet = MovingTransformedPointSetType::New();
    this->m_MovingTransformedPointSet->Initialize();

    typename MovingPointsContainer::ConstIterator It = this->m_MovingPointSet->GetPoints()->Begin();
    while( It != this->m_MovingPointSet->GetPoints()->End() )
      {
      //PointType point = this->m_MovingTransform->TransformPoint( It.Value() );
      //this->m_MovingTransformedPointSet->SetPoint( It.Index(), point );

      // evaluation is perfomed in moving space, so just copy
      this->m_MovingTransformedPointSet->SetPoint( It.Index(), It.Value() );
      ++It;
      }
    this->m_MovingTransformedPointSetTime = this->GetMTime();
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::TransformFixedAndCreateVirtualPointSet() const
{
  // Transform the fixed point set through the virtual domain, and into the moving domain
  if( ( this->GetMTime() > this->m_FixedTransformedPointSetTime )
      || ( this->m_FixedTransform->GetMTime() > this->GetMTime() )
      || ! this->m_FixedTransformedPointSet
      || ! this->m_VirtualTransformedPointSet
      || ( this->m_MovingTransform->GetMTime() > this->GetMTime() ) )
    {
    this->m_FixedTransformPointLocatorsNeedInitialization = true;
    this->m_FixedTransformedPointSet = FixedTransformedPointSetType::New();
    this->m_FixedTransformedPointSet->Initialize();
    this->m_VirtualTransformedPointSet = VirtualPointSetType::New();
    this->m_VirtualTransformedPointSet->Initialize();

    typename FixedTransformType::InverseTransformBasePointer inverseTransform = this->m_FixedTransform->GetInverseTransform();

    typename FixedPointsContainer::ConstIterator It = this->m_FixedPointSet->GetPoints()->Begin();
    while( It != this->m_FixedPointSet->GetPoints()->End() )
      {
      // txf into virtual space
      PointType point = inverseTransform->TransformPoint( It.Value() );
      this->m_VirtualTransformedPointSet->SetPoint( It.Index(), point );
      // txf into moving space
      point = this->m_MovingTransform->TransformPoint( point );
      this->m_FixedTransformedPointSet->SetPoint( It.Index(), point );
      ++It;
      }
    this->m_FixedTransformedPointSetTime = this->GetMTime();
    }
}

template<class TFixedPointSet, class TMovingPointSet>
const typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>::VirtualPointSetType *
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetVirtualTransformedPointSet( void ) const
{
  // First make sure the virtual point set is current.
  this->TransformFixedAndCreateVirtualPointSet();
  return this->m_VirtualTransformedPointSet.GetPointer();
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::InitializePointsLocators() const
{
  if( this->m_FixedTransformPointLocatorsNeedInitialization )
    {
    if( !this->m_FixedTransformedPointSet )
      {
      itkExceptionMacro( "The fixed transformed point set does not exist." );
      }
    if( ! this->m_FixedTransformedPointsLocator )
      {
      this->m_FixedTransformedPointsLocator = PointsLocatorType::New();
      }
    this->m_FixedTransformedPointsLocator->SetPoints( this->m_FixedTransformedPointSet->GetPoints() );
    this->m_FixedTransformedPointsLocator->Initialize();
    }

  if( this->m_MovingTransformPointLocatorsNeedInitialization )
    {
    if( !this->m_MovingTransformedPointSet )
      {
      itkExceptionMacro( "The moving transformed point set does not exist." );
      }
    if( ! this->m_MovingTransformedPointsLocator )
      {
      this->m_MovingTransformedPointsLocator = PointsLocatorType::New();
      }
    this->m_MovingTransformedPointsLocator->SetPoints( this->m_MovingTransformedPointSet->GetPoints() );
    this->m_MovingTransformedPointsLocator->Initialize();
    }
}

/** PrintSelf */
template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Fixed PointSet: " << this->m_FixedPointSet.GetPointer() << std::endl;
  os << indent << "Fixed Transform: " << this->m_FixedTransform.GetPointer() << std::endl;
  os << indent << "Moving PointSet: " << this->m_MovingPointSet.GetPointer() << std::endl;
  os << indent << "Moving Transform: " << this->m_MovingTransform.GetPointer() << std::endl;
}
} // end namespace itk

#endif
