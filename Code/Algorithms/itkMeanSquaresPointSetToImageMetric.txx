/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMeanSquaresPointSetToImageMetric_txx
#define _itkMeanSquaresPointSetToImageMetric_txx

#include "itkMeanSquaresPointSetToImageMetric.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper > 
MeanSquaresPointSetToImageMetric<TTarget,TMapper>
::MeanSquaresPointSetToImageMetric()
{
}

/**
 * Get the match Measure
 */
template < class TTarget, class TMapper > 
MeanSquaresPointSetToImageMetric<TTarget,TMapper>::MeasureType
MeanSquaresPointSetToImageMetric<TTarget,TMapper>
::GetValue( const ParametersType & parameters )
{
  typename TargetType::PointType point;  

  double ReferenceValue;
  double TargetValue;

  typedef  typename  TargetType::PointsContainerConstPointer     
    PointsContainerConstPointerType;

  typedef  typename  TargetType::PointDataContainerConstPointer 
    PointsDataContainerConstPointerType;

  typedef  typename  TargetType::PointsContainer     
    PointsContainerType;

  typedef  typename  TargetType::PointDataContainer 
    PointsDataContainerType;

  typename  PointsContainerType::ConstIterator       pt;
  typename  PointsDataContainerType::ConstIterator   vl;

  TargetConstPointer target = Superclass::GetTarget();

  PointsContainerConstPointerType       points = target->GetPoints();
  PointsDataContainerConstPointerType   data   = target->GetPointData();

  pt = points->Begin();
  vl = data->Begin();

  m_MatchMeasure = 0;

  unsigned int  count = 0;

  MapperPointer mapper = Superclass::GetMapper();
  mapper->GetTransform()->SetParameters( parameters );

  while( pt != points->End()  || vl != data->End() )
    {
    point       = pt.Value();
    TargetValue = vl.Value();

    if( mapper->IsInside( point ) )
      {
      ReferenceValue = mapper->Evaluate();
      count++;
      const double diff = ReferenceValue - TargetValue; 
      m_MatchMeasure += diff * diff; 
      }  

    ++pt;
    ++vl;
    }

  if(count == 0) 
    {
    itkErrorMacro(<< "All the mapped image is outside !" );
    return 100000;
    } 

  // The sign is changed because the optimization method looks for minima
  m_MatchMeasure = m_MatchMeasure / ( count * 1e2 );     
  return m_MatchMeasure;

}

/**
 * Get the Derivative Measure
 */
template < class TTarget, class TMapper> 
const MeanSquaresPointSetToImageMetric<TTarget,TMapper>::DerivativeType &
MeanSquaresPointSetToImageMetric<TTarget,TMapper>
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
  return m_MatchMeasureDerivatives;

}

/**
 * Get both the match Measure and theDerivative Measure 
 */
template < class TTarget, class TMapper > 
void
MeanSquaresPointSetToImageMetric<TTarget,TMapper>
::GetValueAndDerivative(const ParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = GetValue( parameters );
  Derivative = GetDerivative( parameters );
}

} // end namespace itk


#endif
