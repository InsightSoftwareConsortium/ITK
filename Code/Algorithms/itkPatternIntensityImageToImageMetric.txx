/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPatternIntensityImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPatternIntensityImageToImageMetric_txx
#define _itkPatternIntensityImageToImageMetric_txx

#include "itkPatternIntensityImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"



namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper > 
PatternIntensityImageToImageMetric<TTarget,TMapper>
::PatternIntensityImageToImageMetric()
{
}

/**
 * Get the match Measure
 */
template < class TTarget, class TMapper > 
PatternIntensityImageToImageMetric<TTarget,TMapper>::MeasureType
PatternIntensityImageToImageMetric<TTarget,TMapper>
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
      m_MatchMeasure += 1.0 / ( 1.0 + diff * diff ); 
      }  

    ++ti;
    }

  if(count == 0) 
    {
    itkErrorMacro(<< "All the mapped image is outside !" );
    return 100000;
    } 

  // Negative sign to produce a metric to minimize
  m_MatchMeasure = -m_MatchMeasure;     

  return m_MatchMeasure;

}

/**
 * Get the Derivative Measure
 */
template < class TTarget, class TMapper> 
const PatternIntensityImageToImageMetric<TTarget,TMapper>::DerivativeType &
PatternIntensityImageToImageMetric<TTarget,TMapper>
::GetDerivative( const ParametersType & parameters )
{

  const double delta = 0.00011;
  ParametersType testPoint;
  testPoint = parameters;

  for( unsigned int i=0; i<SpaceDimension; i++) 
    {
    testPoint[i] -= delta;
    const MeasureType valuep0 = GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = GetValue( testPoint );
    m_MatchMeasureDerivatives[i] = (valuep1 - valuep0 ) / ( 2 * delta );
    m_MatchMeasureDerivatives[i];
    testPoint[i] = parameters[i];
    }

  return m_MatchMeasureDerivatives;

}

/**
 * Get both the match Measure and theDerivative Measure 
 */
template < class TTarget, class TMapper > 
void
PatternIntensityImageToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(const ParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = GetValue( parameters );
  Derivative = GetDerivative( parameters );
}



} // end namespace itk


#endif
