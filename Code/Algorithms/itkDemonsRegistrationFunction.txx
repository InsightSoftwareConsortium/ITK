/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDemonsRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDemonsRegistrationFunction_txx_
#define _itkDemonsRegistrationFunction_txx_

#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/**
 * Default constructor
 */
template <class TReference, class TTarget, class TDeformationField>
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>
::DemonsRegistrationFunction()
{

  RadiusType r;
  int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_TimeStep = 1.0;
  m_EpsilonDenominator = 1e-9;
  m_Reference = NULL;
  m_Target = NULL;
  m_TargetSpacing = NULL;
  m_TargetOrigin = NULL;
  m_TargetGradientCalculator = GradientCalculatorType::New();


  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_ReferenceInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );


}


/**
 * Standard "PrintSelf" method.
 */
template <class TReference, class TTarget, class TDeformationField>
void
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReferenceIterpolator: ";
  os << m_ReferenceInterpolator.GetPointer() << std::endl;
  os << indent << "TargetGradientCalculator: ";
  os << m_TargetGradientCalculator.GetPointer() << std::endl;
  os << indent << "EpsilonDenominator: ";
  os << m_EpsilonDenominator << std::endl;

}


/**
 * Set the function state values before each iteration
 */
template <class TReference, class TTarget, class TDeformationField>
void
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>
::InitializeIteration()
{
  if( !m_Reference || !m_Target || !m_ReferenceInterpolator )
    {
    itkErrorMacro( << "Reference, Target and/or Interpolator not set" );
    throw ExceptionObject(__FILE__,__LINE__);
    }

  // cache target image information
  m_TargetSpacing    = m_Target->GetSpacing();
  m_TargetOrigin     = m_Target->GetOrigin();

  // setup gradient calculator
  m_TargetGradientCalculator->SetInputImage( m_Target );

  // setup reference interpolator
  m_ReferenceInterpolator->SetInputImage( m_Reference );

}


/**
 * Compute update at a non boundary neighbourhood
 */
template <class TReference, class TTarget, class TDeformationField>
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>::PixelType
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  PixelType update;
  int j;

  IndexType index = it.GetIndex();

  // Get target related information
  double targetValue;
  CovariantVectorType targetGradient;
  double targetGradientSquaredMagnitude = 0;

  // Note: no need to check the index is within
  // target buffer. This is done by the external filter.
  targetValue = (double) m_Target->GetPixel( index );
  for( int j = 0; j < ImageDimension; j++ )
    {
    targetGradient[j] = m_TargetGradientCalculator->EvaluateAtIndex( index, j );
    targetGradientSquaredMagnitude += vnl_math_sqr( targetGradient[j] );
    } 

  // Get reference related information
  double refValue;
  PointType mappedPoint;

  for( j = 0; j < ImageDimension; j++ )
    {
     mappedPoint[j] = double( index[j] ) * m_TargetSpacing[j] + 
      m_TargetOrigin[j];
     mappedPoint[j] += it.GetCenterPixel()[j];
    }
  if( m_ReferenceInterpolator->IsInsideBuffer( mappedPoint ) )
    {
    refValue = m_ReferenceInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    refValue = 0.0;
    }

  // Compute update
  double speedValue = targetValue - refValue;
  double denominator = vnl_math_sqr( speedValue ) + 
    targetGradientSquaredMagnitude;

  if ( denominator < m_EpsilonDenominator )
    {
    for( j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    return update;
    }

  for( j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * targetGradient[j] * m_TargetSpacing[j] / 
      denominator;
    }

  return update;

}



/**
 * Compute update at a boundary neighbourhood
 */
template <class TReference, class TTarget, class TDeformationField>
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>::PixelType
DemonsRegistrationFunction<TReference,TTarget,TDeformationField>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  PixelType update;
  int  j;

  for( j = 0; j < ImageDimension; j++ )
    {
    update[j] = 0.0;
    }

  return update;

}



} // end namespace itk

#endif
