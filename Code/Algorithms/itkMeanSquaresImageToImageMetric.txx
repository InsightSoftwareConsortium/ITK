/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkMeanSquaresImageToImageMetric_txx
#define _itkMeanSquaresImageToImageMetric_txx

#include "itkMeanSquaresImageToImageMetric.h"

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

  std::cout << "GetValue( " << parameters << " ) = ";

  TargetPointer target = Superclass::GetTarget();

  typename TTarget::RegionType  targetRegion = target->GetLargestPossibleRegion();
  itk::Point<double, TTarget::ImageDimension> Point;  

  double ReferenceValue;
  double TargetValue;

  typedef  itk::SimpleImageRegionIterator<TTarget> TargetIteratorType;


  TargetIteratorType ti( target, targetRegion );
  ti.Begin();

  typename TTarget::IndexType index;

  m_MatchMeasure = 0;
  

  unsigned int  count = 0;

  GetMapper()->GetTransformation()->SetParameters( parameters );

  while(!ti.IsAtEnd())
  {
    index = ti.GetIndex();
    for(unsigned int i=0 ; i<TTarget::ImageDimension ; i++)
    {
    Point[i]=index[i];
    }


    if( GetMapper()->IsInside( Point ) )
    {
      ReferenceValue = GetMapper()->Evaluate();
      TargetValue = ti.Get();
      count++;
      const double diff = ReferenceValue - TargetValue; 
      m_MatchMeasure += diff * diff; 
    }
  
   ++ti;
  }

  if(count == 0) 
  {
    std::cout << "All the mapped image is outside !" << std::endl;
    return 100000;
  } 

  m_MatchMeasure = m_MatchMeasure / ( count * 1e2 );     
  std::cout<<" m_MatchMeasure= "<<m_MatchMeasure<<std::endl; 
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
    const MeasureType valuep0 = GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = GetValue( testPoint );
    m_MatchMeasureDerivatives[i] = (valuep1 - valuep0 ) / ( 2 * delta );
    m_MatchMeasureDerivatives[i] /= 1e5;  // FIX this is an arbitrary value
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
  Value      = GetValue( parameters );
  Derivative = GetDerivative( parameters );
}



} // end namespace itk


#endif
