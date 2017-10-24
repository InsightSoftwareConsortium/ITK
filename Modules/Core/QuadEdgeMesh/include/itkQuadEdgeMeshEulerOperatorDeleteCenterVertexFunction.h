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
#ifndef itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h
#define itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction
 * \brief Delete the vertex, connected edges and faces and create a new face
 * in place of the previous vertex' one-ring.
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * >        Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::FaceRefType     FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e);

  PointIdentifier GetOldPointID()
  {
    return ( this->m_OldPointID );
  }

protected:
  QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction() : m_OldPointID(0) {}
  ~QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction);

  PointIdentifier m_OldPointID;
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.hxx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h
