/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkNormalizedCorrelationPointSetToImageMetric_txx
#define _itkNormalizedCorrelationPointSetToImageMetric_txx

#include "itkNormalizedCorrelationPointSetToImageMetric.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper > 
NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>
::NormalizedCorrelationPointSetToImageMetric()
{
}




/**
 * Get the match Measure
 */
template < class TTarget, class TMapper > 
NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>::MeasureType
NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>
::GetValue( const ParametersType & parameters )
{

  std::cout << "GetValue( " << parameters << " ) = ";

  typename TargetType::PointType point;  

  double ReferenceValue;
  double TargetValue;

  typedef  typename  TargetType::PointsContainerPointer     
                                              PointsContainerPointerType;

  typedef  typename  TargetType::PointDataContainerPointer 
                                              PointsDataContainerPointerType;

  typedef  typename  TargetType::PointsContainer     
                                              PointsContainerType;

  typedef  typename  TargetType::PointDataContainer 
                                              PointsDataContainerType;


  typename  PointsContainerType::Iterator       pt;
  typename  PointsDataContainerType::Iterator   vl;

  TargetPointer target = Superclass::GetTarget();

  PointsContainerPointerType       points = target->GetPoints();
  PointsDataContainerPointerType   data   = target->GetPointData();

  pt = points->Begin();
  vl = data->Begin();

  m_MatchMeasure = 0;
  

  unsigned int  count = 0;

  MapperPointer mapper = Superclass::GetMapper();
  mapper->GetTransformation()->SetParameters( parameters );

  double sab = 0.0;
  double saa = 0.0;
  double sbb = 0.0;

  while( pt != points->End()  || vl != data->End() )
  {
    point       = pt.Value();
    TargetValue = vl.Value();

    if( mapper->IsInside( point ) )
    {
      ReferenceValue = mapper->Evaluate();
      count++;
      sab  += ReferenceValue  *  TargetValue;
      saa  += ReferenceValue  *  ReferenceValue;
      sbb  += TargetValue     *  TargetValue;
    }  
  
   ++pt;
   ++vl;
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
const NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>::DerivativeType &
NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>
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
NormalizedCorrelationPointSetToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(const ParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = GetValue( parameters );
  Derivative = GetDerivative( parameters );
}



} // end namespace itk


#endif
