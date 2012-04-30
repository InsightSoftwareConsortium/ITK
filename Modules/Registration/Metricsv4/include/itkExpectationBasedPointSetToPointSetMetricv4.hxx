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
#ifndef __itkExpectationBasedPointSetToPointSetMetricv4_hxx
#define __itkExpectationBasedPointSetToPointSetMetricv4_hxx

#include "itkExpectationBasedPointSetToPointSetMetricv4.h"
#include "itkArray.h"

namespace itk {

/** Constructor */
template<class TFixedPointSet, class TMovingPointSet>
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::ExpectationBasedPointSetToPointSetMetricv4() :
  m_PointSetSigma( 1.0 ),
  m_EvaluationKNeighborhood( 50 )
{
}

/** Destructor */
template<class TFixedPointSet, class TMovingPointSet>
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::~ExpectationBasedPointSetToPointSetMetricv4()
{
}


template<class TFixedPointSet, class TMovingPointSet>
typename ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::MeasureType
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetLocalNeighborhoodValue( const PointType & point ) const
{
  MeasureType localValue = NumericTraits<MeasureType>::Zero;
  const MeasureType preFactor = 1.0 / ( vcl_sqrt( 2 * vnl_math::pi ) * this->m_PointSetSigma );
  const MeasureType denominator = 2.0 * vnl_math_sqr( this->m_PointSetSigma );

  NeighborsIdentifierType neighborhood;
  this->m_FixedTransformedPointsLocator->FindClosestNPoints( point, this->m_EvaluationKNeighborhood, neighborhood );

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_FixedTransformedPointSet->GetPoint( *it );
    const MeasureType distance = point.SquaredEuclideanDistanceTo( neighbor );
    localValue += preFactor * vcl_exp( -distance / denominator );
    }
  return localValue;
}

template<class TFixedPointSet, class TMovingPointSet>
void
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::GetLocalNeighborhoodValueAndDerivative( const PointType & point,
  MeasureType &measure, LocalDerivativeType &localDerivative ) const
{
  Array<MeasureType> measureValues;
  measureValues.SetSize( this->m_EvaluationKNeighborhood );
  measureValues.Fill( 0.0 );

  measure = NumericTraits< MeasureType >::Zero;
  const MeasureType preFactor = 1.0 / ( vcl_sqrt( 2 * vnl_math::pi ) * this->m_PointSetSigma );
  const MeasureType denominator = 2.0 * vnl_math_sqr( this->m_PointSetSigma );

  localDerivative.Fill( 0.0 );

  PointType weightedPoint;
  weightedPoint.Fill( 0.0 );

  NeighborsIdentifierType neighborhood;

  this->m_FixedTransformedPointsLocator->FindClosestNPoints( point, this->m_EvaluationKNeighborhood, neighborhood );

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_FixedTransformedPointSet->GetPoint( *it );
    const MeasureType distance = point.SquaredEuclideanDistanceTo( neighbor );
    measureValues[it - neighborhood.begin()] = preFactor * vcl_exp( -distance / denominator );
    measure += measureValues[it - neighborhood.begin()];
    }

  for( NeighborsIterator it = neighborhood.begin(); it != neighborhood.end(); ++it )
    {
    PointType neighbor = this->m_FixedTransformedPointSet->GetPoint( *it );
    VectorType neighborVector = neighbor.GetVectorFromOrigin();
    weightedPoint += ( neighborVector * measureValues[it - neighborhood.begin()] / measure );
    }

  const MeasureType distance = point.SquaredEuclideanDistanceTo( weightedPoint );

  const MeasureType weight = preFactor * vcl_exp( -distance / denominator ) / measure;

  VectorType force = ( weightedPoint - point ) * weight;

  for( unsigned int d = 0; d < localDerivative.Size(); d++ )
    {
    localDerivative[d] = force[d];
    }
}

template<class TFixedPointSet, class TMovingPointSet>
void
ExpectationBasedPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "PointSetSigma: " << this->m_PointSetSigma << std::endl;
  os << indent << "EvaluateKNeighborhood: " << this->m_EvaluationKNeighborhood << std::endl;
}
} // end namespace itk


#endif
