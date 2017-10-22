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
#ifndef itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h
#define itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorFlipEdgeFunction
 * \brief Flip an edge.
 *
 * The original FlipEdge operator required both faces of the input edge
 * to be triangles (and to be set). This version does not have such requirement.
 * Either or both faces can be polygonal, the org and dest of the edge
 * is then "rotated" around the big polygon that would exist if the two faces
 * of the edge e were joined.
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorFlipEdgeFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorFlipEdgeFunction    Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorFlipEdgeFunction,
               QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  enum EdgeStatusType {
    STANDARD_CONFIG = 0,
    EDGE_NULL,
    MESH_NULL,
    NON_INTERNAL_EDGE,
    NON_TRIANGULAR_RIGHT_FACE,
    NON_TRIANGULAR_LEFT_FACE,
    EXISTING_OPPOSITE_EDGE
    };

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *h);

  // itkGetConstMacro( EdgeStatus, EdgeStatusType );

protected:
  QuadEdgeMeshEulerOperatorFlipEdgeFunction();
  ~QuadEdgeMeshEulerOperatorFlipEdgeFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  EdgeStatusType m_EdgeStatus;

  void CheckStatus(QEType *h);

  OutputType Process(QEType *h);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorFlipEdgeFunction);
};
} // namespace itkQE

#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.hxx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h
