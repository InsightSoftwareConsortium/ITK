/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    std::cerr << "All the mapped image is outside !" << std::endl;
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
