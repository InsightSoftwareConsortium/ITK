/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkNormalizedCorrelationImageToImageMetric_txx
#define _itkNormalizedCorrelationImageToImageMetric_txx

#include "itkNormalizedCorrelationImageToImageMetric.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper > 
NormalizedCorrelationImageToImageMetric<TTarget,TMapper>
::NormalizedCorrelationImageToImageMetric()
{
}




/**
 * Get the match Measure
 */
template < class TTarget, class TMapper > 
NormalizedCorrelationImageToImageMetric<TTarget,TMapper>::MeasureType
NormalizedCorrelationImageToImageMetric<TTarget,TMapper>
::GetValue( const ParametersType & parameters )
{

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
  
  bool insidePoint; 

  unsigned int  count = 0;

  GetMapper()->GetTransformation()->SetParameters( parameters );

  double sab = 0.0;
  double saa = 0.0;
  double sbb = 0.0;

  while(!ti.IsAtEnd())
  {
    index = ti.GetIndex();
    for(unsigned int i=0 ; i<TTarget::ImageDimension ; i++)
    {
    Point[i]=index[i];
    }

    insidePoint = true;

    try {
     ReferenceValue = GetMapper()->Evaluate( Point );
    }

    //If the Mapped Voxel is outside the image
    catch (MapperException) 
    {  
      insidePoint = false;
    }

    if(insidePoint) 
    {
      TargetValue = ti.Get();
      count++;
      sab  += ReferenceValue  *  TargetValue;
      saa  += ReferenceValue  *  ReferenceValue;
      sbb  += TargetValue     *  TargetValue;
    }  
  
   ++ti;
  }

  if(count == 0) 
  {
    std::cout << "All the mapped image is outside !" << std::endl;
    return 100000;
  } 

  // The sign is changed because the optimization method looks for minima
  m_MatchMeasure = -sab / sqrt( saa * sbb );
  std::cout<<"m_MatchMeasure= "<<m_MatchMeasure<<std::endl; 
  return m_MatchMeasure;

}





/**
 * Get the Derivative Measure
 */
template < class TTarget, class TMapper> 
const NormalizedCorrelationImageToImageMetric<TTarget,TMapper>::DerivativeType &
NormalizedCorrelationImageToImageMetric<TTarget,TMapper>
::GetDerivative( const ParametersType & parameters )
{

  const double delta = 0.001;
  ParametersType testPoint;
  testPoint = parameters;

  for( unsigned int i=0; i<SpaceDimension; i++) 
  {
    testPoint[i] -= delta;
    const MeasureType valuep0 = GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = GetValue( testPoint );
    m_MatchMeasureDerivatives[i] = (valuep1 - valuep0 ) / ( 2.0 * delta );
    testPoint[i] = parameters[i];
  }
  std::cout << "NormalizedCorrelation Derivative = ";
  std::cout << m_MatchMeasureDerivatives << std::endl;
  return m_MatchMeasureDerivatives;

}




/**
 * Get both the match Measure and theDerivative Measure 
 */
template < class TTarget, class TMapper > 
void
NormalizedCorrelationImageToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(const ParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = GetValue( parameters );
  Derivative = GetDerivative( parameters );
}



} // end namespace itk


#endif
