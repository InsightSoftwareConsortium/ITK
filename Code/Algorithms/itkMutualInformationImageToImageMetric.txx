/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMutualInformationImageToImageMetric_txx
#define _itkMutualInformationImageToImageMetric_txx

#include "itkMutualInformationImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_sample.h"
#include "vnl/vnl_math.h"
#include "itkGaussianKernelFunction.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper  >
MutualInformationImageToImageMetric<TTarget,TMapper>
::MutualInformationImageToImageMetric()
{

  m_NumberOfSpatialSamples = 0;
  this->SetNumberOfSpatialSamples( 50 );

  m_KernelFunction  = dynamic_cast<KernelFunction*>(
    GaussianKernelFunction::New().GetPointer() );

  m_TargetStandardDeviation = 0.4;
  m_ReferenceStandardDeviation = 0.4;

  m_MinProbability = 0.0001;

  //
  // Following initialization is related to
  // calculating image derivatives
  m_DerivativeCalculator = DerivativeFunctionType::New();

}


template < class TTarget, class TMapper  >
void
MutualInformationImageToImageMetric<TTarget,TMapper>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfSpatialSamples: ";
  os << m_NumberOfSpatialSamples << std::endl;
  os << indent << "TargetStandardDeviation: ";
  os << m_TargetStandardDeviation << std::endl;
  os << indent << "ReferenceStandardDeviation: ";
  os << m_ReferenceStandardDeviation << std::endl;
  os << indent << "KernelFunction: ";
  os << m_KernelFunction.GetPointer() << std::endl;
}


/**
 * Set the number of spatial samples
 */
template < class TTarget, class TMapper  >
void
MutualInformationImageToImageMetric<TTarget,TMapper>
::SetNumberOfSpatialSamples( 
unsigned int num )
{
  if ( num == m_NumberOfSpatialSamples ) return;

  this->Modified();
 
  // clamp to minimum of 1
  m_NumberOfSpatialSamples = ((num > 1) ? num : 1 );

  // resize the storage vectors
  m_SampleA.resize( m_NumberOfSpatialSamples );
  m_SampleB.resize( m_NumberOfSpatialSamples );

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

  typename TargetType::ConstPointer target = this->GetTarget();
  typename MapperType::Pointer mapper = this->GetMapper();

  double range =
   double( target->GetBufferedRegion().GetNumberOfPixels() ) - 1.0;

  typename SpatialSampleContainer::iterator iter;
  typename SpatialSampleContainer::const_iterator end = samples.end();

  bool allOutside = true;

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
      (*iter).TargetPointValue[j] = ( double(index[j]) *
        target->GetSpacing()[j] ) + target->GetOrigin()[j];
      }

    if( mapper->IsInside( (*iter).TargetPointValue ) )
      {
      (*iter).ReferenceValue = mapper->Evaluate();
      allOutside = false;
      }
    else
      {
      (*iter).ReferenceValue = 0;
      }

    }

  if( allOutside )
    {
    // if all the samples mapped to the outside throw an exception
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "MutualInformationImageToImageMetric" );
    err.SetDescription( "All the sampled point mapped to outside of the reference image" );
    throw err;
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

  TargetConstPointer target = this->GetTarget();
  MapperPointer mapper = this->GetMapper();

  if( !target || !mapper )
    {
    m_MatchMeasure = 0;
    return m_MatchMeasure;
    }

  // make sure the mapper has the current parameters
  mapper->GetTransform()->SetParameters( parameters );

  // collect sample set A
  this->SampleTargetDomain( m_SampleA );

  // collect sample set B
  this->SampleTargetDomain( m_SampleB );

  // calculate the mutual information
  double dLogSumTarget = 0.0;
  double dLogSumRef    = 0.0;
  double dLogSumJoint  = 0.0;

  typename SpatialSampleContainer::const_iterator aiter;
  typename SpatialSampleContainer::const_iterator aend = m_SampleA.end();
  typename SpatialSampleContainer::const_iterator biter;
  typename SpatialSampleContainer::const_iterator bend = m_SampleB.end();

  for( biter = m_SampleB.begin() ; biter != bend; ++biter )
    {
    double dSumTarget  = m_MinProbability;
    double dSumRef     = m_MinProbability;
    double dSumJoint   = m_MinProbability;

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

    dLogSumTarget -= log( dSumTarget );
    dLogSumRef    -= log( dSumRef );
    dLogSumJoint  -= log( dSumJoint );

    } // end of sample B loop

  double nsamp   = double( m_NumberOfSpatialSamples );

  double threshold = -0.5 * nsamp * log( m_MinProbability );
  if( dLogSumRef > threshold || dLogSumTarget > threshold ||
      dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "MutualInformationImageToImageMetric" );
    err.SetDescription( "Standard deviation is too small" );
    throw err;
    }

  m_MatchMeasure = dLogSumTarget + dLogSumRef - dLogSumJoint;
  m_MatchMeasure /= nsamp;
  m_MatchMeasure += log( nsamp );

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

  MapperPointer mapper = this->GetMapper();
  TargetConstPointer target = this->GetTarget();

  // check if target and mapper are valid
  if( !target || !mapper )
    {
    value = m_MatchMeasure;
    derivative = m_MatchMeasureDerivatives;
    return;
    }

  // make sure the mapper has the current parameters
  mapper->GetTransform()->SetParameters( parameters );

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
    double dDenominatorRef = m_MinProbability;
    double dDenominatorJoint = m_MinProbability;

    double dSumTarget = m_MinProbability;

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

    dLogSumTarget -= log( dSumTarget );
    dLogSumRef    -= log( dDenominatorRef );
    dLogSumJoint  -= log( dDenominatorJoint );

    // get the image derivative for this B sample
    this->CalculateDerivatives( (*biter).TargetPointValue, derivB );

    for( aiter = m_SampleA.begin(), aditer = m_SampleADerivatives.begin();
      aiter != aend; ++aiter, ++aditer )
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

      weight = ( weightRef - weightJoint );
      weight *= (*biter).ReferenceValue - (*aiter).ReferenceValue;

      m_MatchMeasureDerivatives += ( derivB - (*aditer) ) * weight;

      } // end of sample A loop

    } // end of sample B loop


  double nsamp    = double( m_NumberOfSpatialSamples );

  double threshold = -0.5 * nsamp * log( m_MinProbability );
  if( dLogSumRef > threshold || dLogSumTarget > threshold ||
      dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "MutualInformationImageToImageMetric" );
    err.SetDescription( "Standard deviation is too small" );
    throw err;
    }

  m_MatchMeasure  = dLogSumTarget + dLogSumRef - dLogSumJoint;
  m_MatchMeasure /= nsamp;
  m_MatchMeasure += log( nsamp );

  m_MatchMeasureDerivatives /= nsamp;
  m_MatchMeasureDerivatives /= vnl_math_sqr( m_ReferenceStandardDeviation );

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

  typename MapperType::Pointer mapper = this->GetMapper();
  typename ReferenceType::ConstPointer reference = mapper->GetDomain();

  refPoint = mapper->GetTransform()->TransformPoint( point );

  for( unsigned int j = 0; j < TargetImageDimension; j++ )
    {
    refIndex[j] = (long) vnl_math_rnd( ( refPoint[j] - reference->GetOrigin()[j] ) /
      reference->GetSpacing()[j] );
    }

  CovariantVector<double,TargetImageDimension> imageDerivatives;
  for( unsigned int j = 0; j < TargetImageDimension; j++ )
    {
    imageDerivatives[j] = m_DerivativeCalculator->EvaluateAtIndex( refIndex, j );
    }

  derivatives.Set_vnl_vector( mapper->GetTransform()->
   GetJacobian( point ).GetTranspose() * imageDerivatives.Get_vnl_vector() );

}


} // end namespace itk


#endif

