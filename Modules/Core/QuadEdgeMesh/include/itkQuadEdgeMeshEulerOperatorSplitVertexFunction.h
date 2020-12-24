/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <typename TMesh, typename TQEType>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorSplitVertexFunction
  : public QuadEdgeMeshFunctionBase<TMesh, TQEType *>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshEulerOperatorSplitVertexFunction);

  /** Standard class type aliases. */
  using Self = QuadEdgeMeshEulerOperatorSplitVertexFunction;
  using Superclass = QuadEdgeMeshFunctionBase<TMesh, TQEType *>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  using QEType = TQEType;

  using MeshType = typename Superclass::MeshType;
  using OutputType = typename Superclass::OutputType;
  using VertexRefType = typename MeshType::VertexRefType;
  using EdgeCellType = typename MeshType::EdgeCellType;
  using PointIdentifier = typename MeshType::PointIdentifier;

  /** Evaluate at the specified input position */
  virtual OutputType
  Evaluate(QEType * h, QEType * g);

  PointIdentifier
  GetNewPointID()
  {
    return (this->m_NewPoint);
  }

protected:
  QuadEdgeMeshEulerOperatorSplitVertexFunction() { m_NewPoint = (PointIdentifier)0; }

  ~QuadEdgeMeshEulerOperatorSplitVertexFunction() override = default;

private:
  PointIdentifier m_NewPoint; // stock newly created point ID for user.
};
} // end namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.hxx"

#endif
