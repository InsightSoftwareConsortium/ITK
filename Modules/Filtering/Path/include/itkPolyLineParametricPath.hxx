/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkPolyLineParametricPath_hxx
#define __itkPolyLineParametricPath_hxx

#include "itkPolyLineParametricPath.h"
#include <math.h>

namespace itk
{
template< unsigned int VDimension >
typename PolyLineParametricPath< VDimension >::OutputType
PolyLineParametricPath< VDimension >
::Evaluate(const InputType & input) const
{
  OutputType output;
  PointType  outputPoint;
  VertexType vertex0;
  VertexType vertex1;
  double     fractionOfLineSegment;

  // Handle the endpoint carefully, since there is no following vertex
  if ( input  >=  InputType(m_VertexList->Size() - 1) )
    {
    return m_VertexList->ElementAt(m_VertexList->Size() - 1); // the last vertex
    }

  vertex0 = m_VertexList->ElementAt( int(input) );
  vertex1 = m_VertexList->ElementAt( int(input + 1.0) );

  fractionOfLineSegment = input - int(input);

  outputPoint = vertex0 + ( vertex1 - vertex0 ) * fractionOfLineSegment;

  // For some stupid reason, there is no easy way to cast from a point to a
  // continuous index.
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    output[i] = outputPoint[i];
    }

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
template< unsigned int VDimension >
PolyLineParametricPath< VDimension >
::PolyLineParametricPath()
{
  this->SetDefaultInputStepSize(0.3);
  m_VertexList = VertexListType::New();
}

/**
 * Standard "PrintSelf" method
 */
template< unsigned int VDimension >
void
PolyLineParametricPath< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Verticies:  " << m_VertexList << std::endl;
}
} // end namespace itk

#endif
