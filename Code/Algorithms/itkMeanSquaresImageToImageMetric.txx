/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMeanSquaresImageToImageMetric_txx
#define _itkMeanSquaresImageToImageMetric_txx

#include "itkMeanSquaresImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper > 
MeanSquaresImageToImageMetric<TTarget,TMapper>
::MeanSquaresImageToImageMetric()
{
}

/**
 * Get the match Measure
 */
template < class TTarget, class TMapper > 
MeanSquaresImageToImageMetric<TTarget,TMapper>::MeasureType
MeanSquaresImageToImageMetric<TTarget,TMapper>
::GetValue( const ParametersType & parameters )
{

  TargetConstPointer target = Superclass::GetTarget();

  typename TTarget::RegionType  targetRegion = target->GetLargestPossibleRegion();
  itk::Point<double, TTarget::ImageDimension> Point;  

  double ReferenceValue;
  double TargetValue;

  typedef  itk::ImageRegionConstIteratorWithIndex<TTarget> TargetIteratorType;


  TargetIteratorType ti( target, targetRegion );

  typename TTarget::IndexType index;

  m_MatchMeasure = 0;


  unsigned int  count = 0;

  // cache the mapper so we do not have to make a Get() in the inner loop
  MapperPointer mapper = this->GetMapper();
  mapper->GetTransform()->SetParameters( parameters );

  while(!ti.IsAtEnd())
    {
    index = ti.GetIndex();
    for(unsigned int i=0 ; i<TTarget::ImageDimension ; i++)
      {
      Point[i]=index[i];
      }


    if( mapper->IsInside( Point ) )
      {
      ReferenceValue = mapper->Evaluate();
      TargetValue = ti.Get();
      count++;
      const double diff = ReferenceValue - TargetValue; 
      m_MatchMeasure += diff * diff; 
      }

    ++ti;
    }

  if(count == 0) 
    {
    itkExceptionMacro(<< "All the mapped image is outside !" );
    return 100000;
    } 

  m_MatchMeasure = m_MatchMeasure / count;     
  return m_MatchMeasure;

}





/**
 * Get the Derivative Measure
 */
template < class TTarget, class TMapper> 
const MeanSquaresImageToImageMetric<TTarget,TMapper>::DerivativeType &
MeanSquaresImageToImageMetric<TTarget,TMapper>
::GetDerivative( const ParametersType & parameters )
{

  const double delta = 0.00011;
  ParametersType testPoint;
  testPoint = parameters;

  for( unsigned int i=0; i<SpaceDimension; i++) 
    {
    testPoint[i] -= delta;
    const MeasureType valuep0 = this->GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = this->GetValue( testPoint );
    m_MatchMeasureDerivatives[i] = (valuep1 - valuep0 ) / ( 2 * delta );
    testPoint[i] = parameters[i];
    }

  return m_MatchMeasureDerivatives;

}


/**
 * Get both the match Measure and theDerivative Measure 
 */
template < class TTarget, class TMapper > 
void
MeanSquaresImageToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(const ParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = this->GetValue( parameters );
  Derivative = this->GetDerivative( parameters );
}

} // end namespace itk


#endif
