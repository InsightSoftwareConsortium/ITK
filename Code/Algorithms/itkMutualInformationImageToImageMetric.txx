/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkMutualInformationImageToImageMetric_txx
#define _itkMutualInformationImageToImageMetric_txx

#include "itkMutualInformationImageToImageMetric.h"
#include "vnl/vnl_sample.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper  >
MutualInformationImageToImageMetric<TTarget,TMapper>
::MutualInformationImageToImageMetric()
{

  this->SetNumberOfSpatialSamples( 50 );

  m_KernelFunction  = dynamic_cast<KernelFunction*>(
    GaussianKernelFunction::New().GetPointer() );

  m_TargetStandardDeviation = 0.1;
  m_ReferenceStandardDeviation = 0.1;

  //
  // Following initialization is related to
  // calculating image derivatives
  m_DerivativeCalculator = DerivativeFunctionType::New();

}


/**
 * Uniformly sample the target domain. Each sample consists of:
 *  - the target image value
 *  - the corresponding reference value
 *  - the derivatives of reference intensity wrt to the transform parameters
 */
template < class TTarget, class TMapper  >
void
MutualInformationImageToImageMetric<TTarget,TMapper>
::SampleTargetDomain(
SpatialSampleContainer& samples )
{

  typename TargetType::Pointer target = GetTarget();
  typename MapperType::Pointer mapper = GetMapper();

  double range =
   double( target->GetBufferedRegion().GetNumberOfPixels() ) - 1.0;

  typename SpatialSampleContainer::iterator iter;
  typename SpatialSampleContainer::const_iterator end = samples.end();

  for( iter = samples.begin(); iter != end; ++iter )
    {
    // generate a random number between [0,range)
    unsigned long offset = (unsigned long) vnl_sample_uniform( 0.0, range );

    // translate offset to index in the target domain
    TargetIndexType index = target->ComputeIndex( offset );

    // get target image value
    (*iter).TargetValue = target->GetPixel( index );

    // get reference image value
    for( unsigned int j = 0; j < TargetImageDimension; j++ )
      {
      (*iter).TargetPointValue[j] = index[j];
      }
    try
      {
      (*iter).ReferenceValue = mapper->Evaluate( (*iter).TargetPointValue );
      }
    catch ( MapperException )
      {
      (*iter).ReferenceValue = 0;
      }

    }

}


/**
 * Get the match Measure
 */
template < class TTarget, class TMapper  >
MutualInformationImageToImageMetric<TTarget,TMapper>::MeasureType
MutualInformationImageToImageMetric<TTarget,TMapper>
::GetValue( const ParametersType& parameters )
{

  std::cout << "GetValue( " << parameters << " ) ";

  typename TargetType::Pointer target = GetTarget();
  typename MapperType::Pointer mapper = GetMapper();

  if( !target || !mapper )
    {
    m_MatchMeasure = 0;
    return m_MatchMeasure;
    }

  // make sure the mapper has the current parameters
  mapper->GetTransformation()->SetParameters( parameters );

  // collect sample set A
  this->SampleTargetDomain( m_SampleA );

  // collect sample set B
  this->SampleTargetDomain( m_SampleB );

  // calculate the mutual information
  double dLogSumTarget = 0.0;
  double dLogSumRef    = 0.0;
  double dLogSumJoint  = 0.0;

  double nsamp          = double( m_NumberOfSpatialSamples );
  double targetDenom    = nsamp * m_TargetStandardDeviation;
  double referenceDenom = nsamp * m_ReferenceStandardDeviation;
  double jointDenom     =
    nsamp * m_TargetStandardDeviation * m_ReferenceStandardDeviation;

  typename SpatialSampleContainer::const_iterator aiter;
  typename SpatialSampleContainer::const_iterator aend = m_SampleA.end();
  typename SpatialSampleContainer::const_iterator biter;
  typename SpatialSampleContainer::const_iterator bend = m_SampleB.end();

  for( biter = m_SampleB.begin() ; biter != bend; ++biter )
    {
    double dSumTarget  = 0.0;
    double dSumRef     = 0.0;
    double dSumJoint   = 0.0;

    for( aiter = m_SampleA.begin() ; aiter != aend; ++aiter )
      {
      double valueTarget;
      double valueRef;

      valueTarget = ( (*biter).TargetValue - (*aiter).TargetValue ) /
        m_TargetStandardDeviation;
      valueTarget = m_KernelFunction->Evaluate( valueTarget );

      valueRef = ( (*biter).ReferenceValue - (*aiter).ReferenceValue ) /
        m_ReferenceStandardDeviation;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      dSumTarget += valueTarget;
      dSumRef    += valueRef;
      dSumJoint  += valueTarget * valueRef;

      } // end of sample A loop

    dSumTarget /= targetDenom;
    dSumRef    /= referenceDenom;
    dSumJoint  /= jointDenom;

    dLogSumTarget -= log( dSumTarget );
    dLogSumRef    -= log( dSumRef );
    dLogSumJoint  -= log( dSumJoint );

    } // end of sample B loop

  dLogSumTarget /= nsamp;
  dLogSumRef    /= nsamp;
  dLogSumJoint  /= nsamp;

  m_MatchMeasure = dLogSumTarget + dLogSumRef - dLogSumJoint;

  std::cout << m_MatchMeasure << std::endl;

  return m_MatchMeasure;

}


/**
 * Get the both Value and Derivative Measure
 */
template < class TTarget, class TMapper  >
void
MutualInformationImageToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(
const ParametersType& parameters,
MeasureType& value, 
DerivativeType& derivative)
{

  // reset the derivatives all to zero
  m_MatchMeasureDerivatives.Fill(0);
  m_MatchMeasure = 0;

  typename MapperType::Pointer mapper = GetMapper();
  typename TargetType::Pointer target = GetTarget();

  // check if target and mapper are valid
  if( !target || !mapper )
    {
    value = m_MatchMeasure;
    derivative = m_MatchMeasureDerivatives;
    return;
    }

  // make sure the mapper has the current parameters
  mapper->GetTransformation()->SetParameters( parameters );

  // set the DerivativeCalculator
  m_DerivativeCalculator->SetInputImage( mapper->GetDomain() );

  // collect sample set A
  this->SampleTargetDomain( m_SampleA );

  // collect sample set B
  this->SampleTargetDomain( m_SampleB );


  // calculate the mutual information
  double dLogSumTarget = 0.0;
  double dLogSumRef    = 0.0;
  double dLogSumJoint  = 0.0;

  double nsamp          = double( m_NumberOfSpatialSamples );
  double targetDenom    = nsamp * m_TargetStandardDeviation;
  double referenceDenom = nsamp * m_ReferenceStandardDeviation;
  double jointDenom     =
    nsamp * m_TargetStandardDeviation * m_ReferenceStandardDeviation;

  typename SpatialSampleContainer::iterator aiter;
  typename SpatialSampleContainer::const_iterator aend = m_SampleA.end();
  typename SpatialSampleContainer::iterator biter;
  typename SpatialSampleContainer::const_iterator bend = m_SampleB.end();

  // precalculate all the image derivatives for sample A
  m_SampleADerivatives.resize( m_NumberOfSpatialSamples );

  typename DerivativeContainer::iterator aditer;

  for( aiter = m_SampleA.begin(), aditer = m_SampleADerivatives.begin();
    aiter != aend; ++aiter, ++aditer )
    {
    this->CalculateDerivatives( (*aiter).TargetPointValue, (*aditer) );
    }


  DerivativeType derivB;

  for( biter = m_SampleB.begin(); biter != bend; ++biter )
    {
    double dDenominatorRef = 0.0;
    double dDenominatorJoint = 0.0;

    double dSumTarget = 0.0;

    for( aiter = m_SampleA.begin(); aiter != aend; ++aiter )
      {
      double valueTarget;
      double valueRef;

      valueTarget = ( (*biter).TargetValue - (*aiter).TargetValue )
        / m_TargetStandardDeviation;
      valueTarget = m_KernelFunction->Evaluate( valueTarget );

      valueRef = ( (*biter).ReferenceValue - (*aiter).ReferenceValue )
        / m_ReferenceStandardDeviation;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      dDenominatorRef += valueRef;
      dDenominatorJoint += valueRef * valueTarget;

      dSumTarget += valueTarget;

      } // end of sample A loop

    dLogSumTarget -= log( dSumTarget / targetDenom );
    dLogSumRef -= log( dDenominatorRef / referenceDenom );
    dLogSumJoint -= log( dDenominatorJoint / jointDenom );

    // get the image derivative for this B sample
    this->CalculateDerivatives( (*biter).TargetPointValue, derivB );

    for( aiter = m_SampleA.begin(), aditer = m_SampleADerivatives.begin();
      aiter != aend; ++aiter )
      {
      double valueTarget;
      double valueRef;
      double weightRef;
      double weightJoint;
      double weight;

      valueTarget = ( (*biter).TargetValue - (*aiter).TargetValue ) /
        m_TargetStandardDeviation;
      valueTarget = m_KernelFunction->Evaluate( valueTarget );

      valueRef = ( (*biter).ReferenceValue - (*aiter).ReferenceValue ) /
        m_ReferenceStandardDeviation;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      weightRef = valueRef / dDenominatorRef;
      weightJoint = valueRef * valueTarget / dDenominatorJoint;

      weight = ( weightRef - weightJoint ) / m_ReferenceStandardDeviation;
      weight *= (*biter).ReferenceValue - (*aiter).ReferenceValue;

      m_MatchMeasureDerivatives += ( derivB - (*aditer) ) * weight;

      } // end of sample A loop

    } // end of sample B loop

  dLogSumTarget /= nsamp;
  dLogSumRef /= nsamp;
  dLogSumJoint /= nsamp;

  m_MatchMeasure = dLogSumTarget + dLogSumRef - dLogSumJoint;

  m_MatchMeasureDerivatives /= nsamp;

  value = m_MatchMeasure;
  derivative =  m_MatchMeasureDerivatives;

}


/**
 * Get the match measure derivative
 */
template < class TTarget, class TMapper  >
const MutualInformationImageToImageMetric<TTarget,TMapper>::DerivativeType&
MutualInformationImageToImageMetric<TTarget,TMapper>
::GetDerivative( const ParametersType& parameters )
{
  MeasureType value;
  DerivativeType deriv;
  // call the combined version
  this->GetValueAndDerivative( parameters, value, deriv );

  return m_MatchMeasureDerivatives;
}


/**
 * Calculate derivatives of the image intensity with respect
 * to the transform parmeters.
 *
 * This should really be done by the mapper.
 *
 * This is a temporary solution until this feature is implemented
 * in the mapper. This solution only works for any transform
 * that support GetJacobian()
 */
template < class TTarget, class TMapper  >
void
MutualInformationImageToImageMetric<TTarget,TMapper>
::CalculateDerivatives(
TargetPointType& point,
DerivativeType& derivatives )
{

  TargetPointType refPoint;
  TargetIndexType refIndex;

  typename MapperType::Pointer mapper = GetMapper();

  refPoint = mapper->GetTransformation()->Transform( point );

  for( unsigned int j = 0; j < TargetImageDimension; j++ )
    {
    refIndex[j] = vnl_math_rnd( refPoint[j] );
    }

  Vector<double, TargetImageDimension> imageDerivatives;
  for( int j = 0; j < TargetImageDimension; j++ )
    {
    imageDerivatives[j] = m_DerivativeCalculator->Evaluate( refIndex, j );
    }

  derivatives.Set_vnl_vector( mapper->GetTransformation()->
   GetJacobian( point ).GetTranspose() * imageDerivatives.Get_vnl_vector() );

}


} // end namespace itk


#endif

