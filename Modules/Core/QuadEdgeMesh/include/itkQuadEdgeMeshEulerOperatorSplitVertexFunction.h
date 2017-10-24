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
#ifndef itkQuadEdgeMeshEulerOperatorSplitVertexFunction_h
#define itkQuadEdgeMeshEulerOperatorSplitVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorSplitVertexFunction
 * \brief Split a vertex into two new connected vertices.
 *
 * For two given edges e and f sharing the same dest(), disconnect
 * the two rings, create a new point to be set at f->dest(), and create
 * a new edge between e->Destination() and f->Destination().
 *
 * \sa QuadEdgeMeshEulerOperatorJoinVertexFunction
 *
 * \ingroup QuadEdgeMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorSplitVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitVertexFunction Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType      MeshType;
  typedef typename Superclass::OutputType    OutputType;
  typedef typename MeshType::VertexRefType   VertexRefType;
  typedef typename MeshType::EdgeCellType    EdgeCellType;
  typedef typename MeshType::PointIdentifier PointIdentifier;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e, QEType *f);

  PointIdentifier GetNewPointID()
  {
    return ( this->m_NewPoint );
  }

protected:
  QuadEdgeMeshEulerOperatorSplitVertexFunction()
  {
    m_NewPoint = (PointIdentifier)0;
  }

  ~QuadEdgeMeshEulerOperatorSplitVertexFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorSplitVertexFunction);

  PointIdentifier m_NewPoint; // stock newly created point ID for user.
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.hxx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h
