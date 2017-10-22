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
#ifndef itkQuadEdgeMeshEulerOperatorSplitEdgeFunction_h
#define itkQuadEdgeMeshEulerOperatorSplitEdgeFunction_h

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorSplitEdgeFunction
 * \brief Given Edge is splitted into two and associated faces see their
 * degree increased by one (a triangle is transformed into a quad for
 * example).
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQEType >
class QuadEdgeMeshEulerOperatorSplitEdgeFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitEdgeFunction   Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitEdgeFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType      MeshType;
  typedef typename Superclass::OutputType    OutputType;
  typedef typename MeshType::PointIdentifier PointIdentifier;

  typedef QuadEdgeMeshEulerOperatorSplitVertexFunction< MeshType, QEType > SplitVertex;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e)
  {
    if ( !e )
      {
      itkDebugMacro("Input is not an edge.");
      return ( (QEType *)ITK_NULLPTR );
      }

    if ( !this->m_Mesh )
      {
      itkDebugMacro("No mesh present.");
      return ( (QEType *)ITK_NULLPTR );
      }

    m_SplitVertex->SetInput(this->m_Mesh);
    return ( m_SplitVertex->Evaluate( e->GetLprev(), e->GetSym() ) );
  }

  const PointIdentifier GetNewPointID()
  {
    return ( m_SplitVertex->GetNewPointID() );
  }

protected:
  QuadEdgeMeshEulerOperatorSplitEdgeFunction()
  {
    m_SplitVertex = SplitVertex::New();
  }

  ~QuadEdgeMeshEulerOperatorSplitEdgeFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorSplitEdgeFunction);

  typename SplitVertex::Pointer m_SplitVertex;
};
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h
