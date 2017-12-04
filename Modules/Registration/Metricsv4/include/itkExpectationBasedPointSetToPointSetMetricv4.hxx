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
#ifndef itkExpectationBasedPointSetToPointSetMetricv4_hxx
#define itkExpectationBasedPointSetToPointSetMetricv4_hxx

#include "itkExpectationBasedPointSetToPointSetMetricv4.h"
#include "itkArray.h"

namespace itk {

/** Constructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::ExpectationBasedPointSetToPointSetMetricv4() :
  m_PointSetSigma( 1.0 ),
  m_PreFactor( 0.0 ),
  m_Denominator( 0.0 ),
  m_EvaluationKNeighborhood( 50 )
{
}

/** Destructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::~ExpectationBasedPointSetToPointSetMetricv4()
{
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::Initialize( void )
{
  Superclass::Initialize();

  if( this->m_PointSetSigma <= NumericTraits<CoordRepType>::epsilon() )
    {
    itkExceptionMacro("m_PointSetSigma is too small. <= epsilon");
    }
  this->m_PreFactor = 1.0 / ( std::sqrt( 2 * itk::Math::pi ) * this->m_PointSetSigma );
  this->m_Denominator = 2.0 * itk::Math::sqr( this->m_PointSetSigma );
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::MeasureType
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValue( const PointType & point, const PixelType & itkNotUsed( pixel ) ) const
{
  MeasureType localValue = NumericTraits<MeasureType>::ZeroValue();

  NeighborsIdentifierType neighborhood;
  this->m_MovingTransformedPointsLocator->FindClosestNPoints( point, this->m_EvaluationKNeighborhood, neighborhood );

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_MovingTransformedPointSet->GetPoint( *it );
    const MeasureType distance = point.SquaredEuclideanDistanceTo( neighbor );
    localValue -= this->m_PreFactor * std::exp( -distance / this->m_Denominator );
    }

  return localValue;
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValueAndDerivative( const PointType & point,
  MeasureType &measure, LocalDerivativeType &localDerivative, const PixelType & itkNotUsed( pixel ) ) const
{
  Array<MeasureType> measureValues;
  measureValues.SetSize( this->m_EvaluationKNeighborhood );
  measureValues.Fill( 0.0 );

  measure = NumericTraits< MeasureType >::ZeroValue();

  localDerivative.Fill( 0.0 );

  PointType weightedPoint;
  weightedPoint.Fill( 0.0 );

  NeighborsIdentifierType neighborhood;

  this->m_MovingTransformedPointsLocator->FindClosestNPoints( point, this->m_EvaluationKNeighborhood, neighborhood );

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_MovingTransformedPointSet->GetPoint( *it );
    const MeasureType distance = point.SquaredEuclideanDistanceTo( neighbor );
    measureValues[it - neighborhood.begin()] = -this->m_PreFactor * std::exp( -distance / this->m_Denominator );
    measure += measureValues[it - neighborhood.begin()];
    }

  if ( std::fabs(measure) <= NumericTraits<MeasureType>::epsilon() )
    {
    return;
    }

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_MovingTransformedPointSet->GetPoint( *it );
    VectorType neighborVector = neighbor.GetVectorFromOrigin();
    weightedPoint += ( neighborVector * measureValues[it - neighborhood.begin()] / measure );
    }

  const MeasureType distance = point.SquaredEuclideanDistanceTo( weightedPoint );

  const MeasureType weight = this->m_PreFactor * std::exp( -distance / this->m_Denominator ) / -measure;

  VectorType force = ( weightedPoint - point ) * weight;

  for( unsigned int d = 0; d < localDerivative.Size(); d++ )
    {
    localDerivative[d] = force[d];
    }
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename LightObject::Pointer
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::InternalClone( void ) const
{
  typename Self::Pointer rval = Self::New();
  rval->SetMovingPointSet( this->m_MovingPointSet );
  rval->SetFixedPointSet( this->m_FixedPointSet );
  rval->SetPointSetSigma( this->m_PointSetSigma );
  rval->SetEvaluationKNeighborhood( this->m_EvaluationKNeighborhood );

  return rval.GetPointer();
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "PointSetSigma: " << this->m_PointSetSigma << std::endl;
  os << indent << "EvaluateKNeighborhood: " << this->m_EvaluationKNeighborhood << std::endl;
}
} // end namespace itk


#endif
