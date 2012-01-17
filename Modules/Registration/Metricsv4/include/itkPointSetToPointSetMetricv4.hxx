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

  // Set transforms to identity as default

  typedef IdentityTransform<CoordRepType, FixedPointDimension>
    FixedIdentityTransformType;
  typename FixedIdentityTransformType::Pointer fixedIdentityTransform =
    FixedIdentityTransformType::New();
  fixedIdentityTransform->SetIdentity();

  this->m_FixedTransform = fixedIdentityTransform;

  typedef IdentityTransform<CoordRepType, MovingPointDimension>
    MovingIdentityTransformType;
  typename MovingIdentityTransformType::Pointer movingIdentityTransform =
    MovingIdentityTransformType::New();
  movingIdentityTransform->SetIdentity();

  this->m_MovingTransform = movingIdentityTransform;
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
  if ( !this->m_FixedTransform )
    {
    itkExceptionMacro( "Fixed transform is not present" );
    }

  if ( !this->m_MovingTransform )
    {
    itkExceptionMacro( "Moving transform is not present" );
    }

  if ( !this->m_FixedPointSet )
    {
    itkExceptionMacro( "Fixed point set is not present" );
    }

  if ( !this->m_MovingPointSet )
    {
    itkExceptionMacro( "Moving point set is not present" );
    }

  // If the PointSet is provided by a source, update the source.
  if( this->m_MovingPointSet->GetSource() )
    {
    this->m_MovingPointSet->GetSource()->Update();
    }
  this->TransformMovingPointSet();

  // If the point set is provided by a source, update the source.
  if( this->m_FixedPointSet->GetSource() )
    {
    this->m_FixedPointSet->GetSource()->Update();
    }
  this->TransformFixedPointSet();

  // Initialize the point locators
  this->InitializePointsLocators();
}

template<class TFixedPointSet, class TMovingPointSet>
unsigned int
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetNumberOfComponents() const
{
  unsigned long numberOfComponents = 0;

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_FIXED ||
    this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    numberOfComponents += this->m_FixedTransformedPointSet->GetNumberOfPoints();
    }

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_MOVING ||
    this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    numberOfComponents += this->m_MovingTransformedPointSet->GetNumberOfPoints();
    }

  return numberOfComponents;
}

template<class TFixedPointSet, class TMovingPointSet>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>::MeasureType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetValue() const
{
  MeasureType measure = 0.0;

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_FIXED ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {

    PointsConstIterator It = this->m_FixedTransformedPointSet->GetPoints()->Begin();

    while( It != this->m_FixedTransformedPointSet->GetPoints()->End() )
      {
      measure += this->GetLocalNeighborhoodValue( It.Value() );
      ++It;
      }
    }

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_MOVING ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    PointsConstIterator It = this->m_MovingTransformedPointSet->GetPoints()->Begin();

    while( It != this->m_MovingTransformedPointSet->GetPoints()->End() )
      {
      measure += this->GetLocalNeighborhoodValue( It.Value() );
      ++It;
      }
    }
  measure /= static_cast<MeasureType>( this->GetNumberOfComponents() );

  return measure;
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetDerivative( DerivativeType & derivative ) const
{
  derivative.SetSize( this->GetNumberOfComponents() * PointDimension );
  derivative.Fill( 0 );

  unsigned long index = 0;

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_FIXED ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    PointsConstIterator It = this->m_FixedTransformedPointSet->GetPoints()->Begin();
    while( It != this->m_FixedTransformedPointSet->GetPoints()->End() )
      {
      LocalDerivativeType localDerivative = this->GetLocalNeighborhoodDerivative( It.Value() );
      for( unsigned int d = 0; d < PointDimension; ++d )
        {
        derivative(index) = localDerivative[d];
        ++index;
        }

      ++It;
      }
    }

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_MOVING ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    PointsConstIterator It = this->m_MovingTransformedPointSet->GetPoints()->Begin();
    while( It != this->m_MovingTransformedPointSet->GetPoints()->End() )
      {
      LocalDerivativeType localDerivative =
        this->GetLocalNeighborhoodDerivative( It.Value() );
      for( unsigned int d = 0; d < PointDimension; ++d )
        {
        derivative(index) = localDerivative[d];
        ++index;
        }
      ++It;
      }
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const
{
  derivative.SetSize( this->GetNumberOfComponents() * PointDimension );
  derivative.Fill( 0 );

  value = 0.0;

  unsigned long index = 0;

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_FIXED ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    PointsConstIterator It = this->m_FixedTransformedPointSet->GetPoints()->Begin();
    while( It != this->m_FixedTransformedPointSet->GetPoints()->End() )
      {
      MeasureType localValue = 0.0;
      LocalDerivativeType localDerivative;

      this->GetLocalNeighborhoodValueAndDerivative( It.Value(), localValue, localDerivative );

      for( unsigned int d = 0; d < PointDimension; ++d )
        {
        derivative(index) = localDerivative[d];
        ++index;
        }

      value += localValue;

      ++It;
      }
    }

  if( this->GetGradientSource() == Superclass::GRADIENT_SOURCE_MOVING ||
      this->GetGradientSource() == Superclass::GRADIENT_SOURCE_BOTH )
    {
    PointsConstIterator It = this->m_MovingTransformedPointSet->GetPoints()->Begin();

    while( It != this->m_MovingTransformedPointSet->GetPoints()->End() )
      {
      MeasureType localValue = 0.0;
      LocalDerivativeType localDerivative;

      this->GetLocalNeighborhoodValueAndDerivative( It.Value(), localValue, localDerivative );

      for( unsigned int d = 0; d < PointDimension; ++d )
        {
        derivative(index) = localDerivative[d];
        ++index;
        }

      value += localValue;

      ++It;
      }
    }

  value /= static_cast<MeasureType>( this->GetNumberOfComponents() );
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
::TransformMovingPointSet()
{
  if( ( this->m_MovingTransform->GetMTime() > this->GetMTime() ) ||
    !this->m_MovingTransformedPointSet )
    {
    this->m_MovingTransformedPointSet = MovingTransformedPointSetType::New();
    this->m_MovingTransformedPointSet->Initialize();

    typename MovingPointsContainer::ConstIterator It =
      this->m_MovingPointSet->GetPoints()->Begin();
    while( It != this->m_MovingPointSet->GetPoints()->End() )
      {
      PointType point = this->m_MovingTransform->TransformPoint( It.Value() );
      this->m_MovingTransformedPointSet->SetPoint( It.Index(), point );
      ++It;
      }
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::TransformFixedPointSet()
{
  if( ( this->m_FixedTransform->GetMTime() > this->GetMTime() ) ||
    !this->m_FixedTransformedPointSet )
    {
    this->m_FixedTransformedPointSet = FixedTransformedPointSetType::New();
    this->m_FixedTransformedPointSet->Initialize();

    typename FixedPointsContainer::ConstIterator It =
      this->m_FixedPointSet->GetPoints()->Begin();

    while( It != this->m_FixedPointSet->GetPoints()->End() )
      {
      PointType point = this->m_FixedTransform->TransformPoint( It.Value() );
      this->m_FixedTransformedPointSet->SetPoint( It.Index(), point );
      ++It;
      }
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::InitializePointsLocators()
{
  if( !this->m_FixedTransformedPointSet )
    {
    itkExceptionMacro( "The fixed transformed point set does not exist." );
    }

  if( !this->m_MovingTransformedPointSet )
    {
    itkExceptionMacro( "The moving transformed point set does not exist." );
    }

  this->m_FixedTransformedPointsLocator = PointsLocatorType::New();
  this->m_FixedTransformedPointsLocator->SetPoints(
    this->m_FixedTransformedPointSet->GetPoints() );
  this->m_FixedTransformedPointsLocator->Initialize();

  this->m_MovingTransformedPointsLocator = PointsLocatorType::New();
  this->m_MovingTransformedPointsLocator->SetPoints(
    this->m_MovingTransformedPointSet->GetPoints() );
  this->m_MovingTransformedPointsLocator->Initialize();
}

template<class TFixedPointSet, class TMovingPointSet>
unsigned int
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetNumberOfParameters() const
{
  return this->m_MovingTransform->GetNumberOfParameters();
}

template<class TFixedPointSet, class TMovingPointSet>
const typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>::ParametersType &
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetParameters() const
{
  return this->m_MovingTransform->GetParameters();
}

template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::SetParameters( ParametersType & params )
{
  this->m_MovingTransform->SetParametersByValue( params );
}

template<class TFixedPointSet, class TMovingPointSet>
unsigned int
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetNumberOfLocalParameters() const
{
  return this->m_MovingTransform->GetNumberOfLocalParameters();
}

template<class TFixedPointSet, class TMovingPointSet>
bool
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::HasLocalSupport() const
{
  return this->m_MovingTransform->HasLocalSupport();
}

/*
 * UpdateParameters
 */
template<class TFixedPointSet, class TMovingPointSet>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::UpdateTransformParameters( DerivativeType & derivative, ParametersValueType factor )
{
  /* Rely on transform::UpdateTransformParameters to verify proper
   * size of derivative */
  this->m_MovingTransform->UpdateTransformParameters( derivative, factor );
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
