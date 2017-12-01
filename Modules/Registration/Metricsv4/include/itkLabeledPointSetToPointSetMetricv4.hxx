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
#ifndef itkLabeledPointSetToPointSetMetricv4_hxx
#define itkLabeledPointSetToPointSetMetricv4_hxx

#include "itkLabeledPointSetToPointSetMetricv4.h"

#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"

#include <algorithm>

namespace itk
{

/** Constructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::LabeledPointSetToPointSetMetricv4()
{
  typedef EuclideanDistancePointSetToPointSetMetricv4<FixedPointSetType, MovingPointSetType, TInternalComputationValueType> DefaultMetricType;
  typename DefaultMetricType::Pointer euclideanMetric = DefaultMetricType::New();
  this->m_PointSetMetric = euclideanMetric;

  this->m_UsePointSetData = true;
}

/** Destructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::~LabeledPointSetToPointSetMetricv4()
{
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::Initialize( void )
{
  if( !this->m_FixedPointSet->GetPointData() || this->m_FixedPointSet->GetPoints()->Size() != this->m_FixedPointSet->GetPointData()->Size() ||
    !this->m_MovingPointSet->GetPointData() || this->m_MovingPointSet->GetPoints()->Size() != this->m_MovingPointSet->GetPointData()->Size() )
    {
    itkExceptionMacro( "Each point of the point set must be associated with a label." );
    }

  this->DetermineCommonPointSetLabels();

  // Create point set metric instantiations for each label

  typename LabelSetType::const_iterator it;
  for( it = this->m_CommonPointSetLabels.begin(); it != this->m_CommonPointSetLabels.end(); ++it )
    {
    typename PointSetMetricType::Pointer metric = dynamic_cast<PointSetMetricType *>( this->m_PointSetMetric->Clone().GetPointer() );
    if( metric.IsNull() )
      {
      itkExceptionMacro( "The metric pointer clone is ITK_NULLPTR." );
      }

    FixedPointSetPointer fixedPointSet = this->GetLabeledFixedPointSet( *it );
    MovingPointSetPointer movingPointSet = this->GetLabeledMovingPointSet( *it );

    metric->SetFixedPointSet( fixedPointSet );
    metric->SetMovingPointSet( movingPointSet );

    metric->SetFixedTransform( this->GetModifiableFixedTransform() );
    metric->SetMovingTransform( this->GetModifiableMovingTransform() );

    metric->SetCalculateValueAndDerivativeInTangentSpace(
      this->GetCalculateValueAndDerivativeInTangentSpace() );
    metric->SetStoreDerivativeAsSparseFieldForLocalSupportTransforms(
      this->GetStoreDerivativeAsSparseFieldForLocalSupportTransforms() );

    metric->Initialize();

    this->m_PointSetMetricClones.push_back( metric );
    }
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::MeasureType
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValue( const PointType & point, const LabelType & label ) const
{
  typename LabelSetType::const_iterator labelIt = std::find( this->m_CommonPointSetLabels.begin(), this->m_CommonPointSetLabels.end(), label );
  if( labelIt == this->m_CommonPointSetLabels.end() )
    {
    itkExceptionMacro( "Label not found in common label set" );
    }
  else
    {
    unsigned int labelIndex = labelIt - this->m_CommonPointSetLabels.begin();
    MeasureType value = this->m_PointSetMetricClones[labelIndex]->GetLocalNeighborhoodValue( point, label );
    return value;
    }
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValueAndDerivative( const PointType & point,
  MeasureType &measure, LocalDerivativeType & localDerivative, const LabelType & label ) const
{
  typename LabelSetType::const_iterator labelIt = std::find( this->m_CommonPointSetLabels.begin(), this->m_CommonPointSetLabels.end(), label );
  if( labelIt == this->m_CommonPointSetLabels.end() )
    {
    itkExceptionMacro( "Label not found in common label set" );
    }
  else
    {
    unsigned int labelIndex = labelIt - this->m_CommonPointSetLabels.begin();
    this->m_PointSetMetricClones[labelIndex]->GetLocalNeighborhoodValueAndDerivative( point, measure, localDerivative, label );
    }
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::FixedPointSetPointer
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLabeledFixedPointSet( const LabelType label ) const
{
  typename FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();
  fixedPointSet->Initialize();

  typename FixedPointSetType::PointIdentifier count = NumericTraits<PointIdentifier>::ZeroValue();

  typename FixedPointSetType::PointsContainerConstIterator It = this->m_FixedPointSet->GetPoints()->Begin();
  typename FixedPointSetType::PointDataContainerIterator ItD = this->m_FixedPointSet->GetPointData()->Begin();
  while( It != this->m_FixedPointSet->GetPoints()->End() )
    {
    if( label == ItD.Value() )
      {
      fixedPointSet->SetPoint( count++, It.Value() );
      }
    ++It;
    ++ItD;
    }

  return fixedPointSet;
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::MovingPointSetPointer
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLabeledMovingPointSet( const LabelType label ) const
{
  typename MovingPointSetType::Pointer movingPointSet = MovingPointSetType::New();
  movingPointSet->Initialize();

  typename MovingPointSetType::PointIdentifier count = NumericTraits<PointIdentifier>::ZeroValue();

  typename MovingPointSetType::PointsContainerConstIterator It = this->m_MovingPointSet->GetPoints()->Begin();
  typename MovingPointSetType::PointDataContainerIterator ItD = this->m_MovingPointSet->GetPointData()->Begin();
  while( It != this->m_MovingPointSet->GetPoints()->End() )
    {
    if( label == ItD.Value() )
      {
      movingPointSet->SetPoint( count++, It.Value() );
      }
    ++It;
    ++ItD;
    }

  return movingPointSet;
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::DetermineCommonPointSetLabels()
{
  this->m_FixedPointSetLabels.clear();
  this->m_MovingPointSetLabels.clear();
  this->m_CommonPointSetLabels.clear();

  if( this->m_FixedPointSet->GetNumberOfPoints() > 0 )
    {
    typename FixedPointSetType::PointDataContainerIterator It = this->m_FixedPointSet->GetPointData()->Begin();
    while( It != this->m_FixedPointSet->GetPointData()->End() )
      {
      if( std::find( this->m_FixedPointSetLabels.begin(), this->m_FixedPointSetLabels.end(), It.Value() ) == this->m_FixedPointSetLabels.end() )
        {
        this->m_FixedPointSetLabels.push_back( It.Value() );
        }
      ++It;
      }
    }
  std::sort( this->m_FixedPointSetLabels.begin(), this->m_FixedPointSetLabels.end() );

  if( this->m_MovingPointSet->GetNumberOfPoints() > 0 )
    {
    typename MovingPointSetType::PointDataContainerIterator It = this->m_MovingPointSet->GetPointData()->Begin();
    while( It != this->m_MovingPointSet->GetPointData()->End() )
      {
      if( std::find( this->m_MovingPointSetLabels.begin(), this->m_MovingPointSetLabels.end(), It.Value() ) == this->m_MovingPointSetLabels.end() )
        {
        this->m_MovingPointSetLabels.push_back( It.Value() );
        }
      ++It;
      }
    }
  std::sort( this->m_MovingPointSetLabels.begin(), this->m_MovingPointSetLabels.end() );

  LabelSetType uncommonLabelSet;

  typename LabelSetType::const_iterator itF;
  for( itF = this->m_FixedPointSetLabels.begin(); itF != this->m_FixedPointSetLabels.end(); ++itF )
    {
    if( std::find( this->m_MovingPointSetLabels.begin(), this->m_MovingPointSetLabels.end(), *itF ) != this->m_MovingPointSetLabels.end() )
      {
      this->m_CommonPointSetLabels.push_back( *itF );
      }
    else
      {
      uncommonLabelSet.push_back( *itF );
      }
    }

  if( uncommonLabelSet.size() > 0 )
    {
    itkWarningMacro( "The label sets are not bijective." );
    }
}

/** PrintSelf method */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
LabeledPointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << "Fixed label set: ";
  typename LabelSetType::const_iterator itF;
  for( itF = this->m_FixedPointSetLabels.begin(); itF != this->m_FixedPointSetLabels.end(); ++itF )
    {
    os << *itF << " ";
    }
  os << std::endl;

  os << "Moving label set: ";
  typename LabelSetType::const_iterator itM;
  for( itM = this->m_MovingPointSetLabels.begin(); itM != this->m_MovingPointSetLabels.end(); ++itM )
    {
    os << *itM << " ";
    }
  os << std::endl;
}

} // end namespace itk

#endif
