/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolyLineParametricPath.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkPolyLineParametricPath_txx
#define _itkPolyLineParametricPath_txx

#include "itkPolyLineParametricPath.h"
#include <math.h>



namespace itk
{

template<unsigned int VDimension>
typename PolyLineParametricPath<VDimension>::OutputType
PolyLineParametricPath<VDimension>
::Evaluate( const InputType & input ) const
{
  OutputType output;
  PointType  outputPoint;
  VertexType vertex0;
  VertexType vertex1;
  double     fractionOfLineSegment;
  
  // Handle the endpoint carefully, since there is no following vertex
  if(  input  >=  InputType( m_VertexList->Size() - 1 )  )
    {
    return m_VertexList->ElementAt(m_VertexList->Size() - 1); // the last vertex
    }
  
  vertex0 = m_VertexList->ElementAt( int(input) );
  vertex1 = m_VertexList->ElementAt( int(input+1.0) );
  
  fractionOfLineSegment = input - int(input);
  
  outputPoint = vertex0 + (vertex1-vertex0)*fractionOfLineSegment;
  
  // For some stupid reason, there is no easy way to cast from a point to a
  // continuous index.
  for( unsigned int i=0; i<VDimension; i++ ) { output[i] = outputPoint[i]; }
  
  return output;
}



//template<unsigned int VDimension>
//typename PolyLineParametricPath<VDimension>::VectorType
//PolyLineParametricPath<VDimension>
//::EvaluateDerivative(const InputType & input) const
//{
//}



/**
 * Constructor
 */
template <unsigned int VDimension>
PolyLineParametricPath<VDimension>
::PolyLineParametricPath()
{
  m_DefaultInputStepSize = 0.3;
  m_VertexList = VertexListType::New();
  this->Modified();
}



/**
 * Standard "PrintSelf" method
 */
template <unsigned int VDimension>
void
PolyLineParametricPath<VDimension>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Verticies:  " << m_VertexList << std::endl;
}



} // end namespaceitk

#endif
