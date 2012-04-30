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

  this->m_FixedTransformedPointsLocator = NULL;
  this->m_MovingTransformedPointsLocator = NULL;

  this->m_MovingTransformPointLocatorsNeedInitialization = false;
  this->m_FixedTransformPointLocatorsNeedInitialization = false;
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
  Superclass::Initialize();

  if ( !this->m_FixedPointSet )
    {
    itkExceptionMacro( "Fixed point set is not present" );
    }

  if ( !this->m_MovingPointSet )
    {
    itkExceptionMacro( "Moving point set is not present" );
    }

  // We don't know how to support gradient source of type fixed
  if( this->GetGradientSourceIncludesFixed() )
    {
    itkExceptionMacro("GradientSource includes GRADIENT_SOURCE_FIXED. Not supported.");
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

  // Call this now for derived classes that may need
  // a member to be initialized during Initialize().
  this->InitializeForIteration();
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::InitializeForIteration() const
{
  this->TransformMovingPointSet();
  this->TransformFixedPointSet();
  this->InitializePointsLocators();
}

template<class TFixedPointSet, class TMovingPointSet>
SizeValueType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetNumberOfComponents() const
{
  return this->m_MovingTransformedPointSet->GetNumberOfPoints();
}

template<class TFixedPointSet, class TMovingPointSet>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>::MeasureType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetValue() const
{
  this->InitializeForIteration();

  MeasureType measure = 0.0;

  PointsConstIterator It = this->m_MovingTransformedPointSet->GetPoints()->Begin();

  while( It != this->m_MovingTransformedPointSet->GetPoints()->End() )
    {
    measure += this->GetLocalNeighborhoodValue( It.Value() );
    ++It;
    }
  measure /= static_cast<MeasureType>( this->GetNumberOfComponents() );

  this->m_Value = measure;

  return measure;
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
  derivative.Fill( 0 );

  value = NumericTraits<MeasureType>::Zero;
  MovingTransformJacobianType  jacobian( MovingPointDimension, this->GetNumberOfLocalParameters() );

  DerivativeType localTransformDerivative( this->GetNumberOfLocalParameters() );
  localTransformDerivative.Fill( NumericTraits<DerivativeValueType>::Zero );

  PointsConstIterator It = this->m_MovingTransformedPointSet->GetPoints()->Begin();
  PointsConstIterator end = this->m_MovingTransformedPointSet->GetPoints()->End();
  while( It != end )
    {
    MeasureType pointValue = NumericTraits<MeasureType>::Zero;
    LocalDerivativeType pointDerivative;

    if( calculateValue )
      {
      this->GetLocalNeighborhoodValueAndDerivative( It.Value(), pointValue, pointDerivative );
      value += pointValue;
      }
    else
      {
      pointDerivative = this->GetLocalNeighborhoodDerivative( It.Value() );
      }

    this->GetMovingTransform()->ComputeJacobianWithRespectToParameters( It.Value(), jacobian );
    // Map into parameter space
    for ( NumberOfParametersType par = 0; par < this->GetNumberOfLocalParameters(); par++ )
      {
      if( this->HasLocalSupport() )
        {
        localTransformDerivative[par] = NumericTraits<DerivativeValueType>::Zero;
        }
      for( DimensionType d = 0; d < PointDimension; ++d )
        {
        localTransformDerivative[par] += jacobian(d, par) * pointDerivative[d];
        }
      }
    // For local-support transforms, store the result per-point
    if( this->HasLocalSupport() )
      {
      itkExceptionMacro("Local Support: TODO");
      }
    ++It;
    }

  // For global-support transforms, average the accumulated derivative result
  if( ! this->HasLocalSupport() )
    {
    derivative = localTransformDerivative / this->GetNumberOfComponents();
    }
  derivative *= -1.0;
  value /= static_cast<MeasureType>( this->GetNumberOfComponents() );
  this->m_Value = value;
}

template<class TFixedPointSet, class TMovingPointSet>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::LocalDerivativeType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetLocalNeighborhoodDerivative( const PointType & point ) const
{
  MeasureType measure;
  LocalDerivativeType localDerivative;
  this->GetLocalNeighborhoodValueAndDerivative( point, measure, localDerivative );
  return localDerivative;
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::TransformMovingPointSet() const
{
  // Transform the moving point set with the moving transform.
  // We calculate the value and derivatives in the moving space.
  if( ( this->m_MovingTransform->GetMTime() > this->GetMTime() ) || !this->m_MovingTransformedPointSet )
    {
    this->m_MovingTransformPointLocatorsNeedInitialization = true;
    if( !this->m_MovingTransformedPointSet )
      {
      this->m_MovingTransformedPointSet = MovingTransformedPointSetType::New();
      this->m_MovingTransformedPointSet->Initialize();
      }

    typename MovingPointsContainer::ConstIterator It = this->m_MovingPointSet->GetPoints()->Begin();
    while( It != this->m_MovingPointSet->GetPoints()->End() )
      {
      //PointType point = this->m_MovingTransform->TransformPoint( It.Value() );
      //this->m_MovingTransformedPointSet->SetPoint( It.Index(), point );

      // evaluation is perfomed in moving space, so just copy
      this->m_MovingTransformedPointSet->SetPoint( It.Index(), It.Value() );
      ++It;
      }
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::TransformFixedPointSet() const
{
  // Transform the fixed point set into the moving domain
  if( ( this->m_FixedTransform->GetMTime() > this->GetMTime() ) || ! this->m_FixedTransformedPointSet
        || ( this->m_MovingTransform->GetMTime() > this->GetMTime() ) )
    {
    this->m_FixedTransformPointLocatorsNeedInitialization = true;
    if( !this->m_FixedTransformedPointSet )
      {
      this->m_FixedTransformedPointSet = FixedTransformedPointSetType::New();
      this->m_FixedTransformedPointSet->Initialize();
      }

    typename FixedTransformType::InverseTransformBasePointer inverseTransform = this->m_FixedTransform->GetInverseTransform();

    typename FixedPointsContainer::ConstIterator It = this->m_FixedPointSet->GetPoints()->Begin();

    while( It != this->m_FixedPointSet->GetPoints()->End() )
      {
      PointType point = inverseTransform->TransformPoint( It.Value() );
      // txf into moving space
      point = this->m_MovingTransform->TransformPoint( point );
      this->m_FixedTransformedPointSet->SetPoint( It.Index(), point );
      ++It;
      }
    }
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
