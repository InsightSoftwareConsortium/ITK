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
#ifndef itkQuadEdgeMeshBoundaryEdgesMeshFunction_h
#define itkQuadEdgeMeshBoundaryEdgesMeshFunction_h

#include "itkFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshBoundaryEdgesMeshFunction
 *
 * \brief Build a list of references to edges (as \ref GeometricalQuadEdge::RawPointer)
 *        each one representing a different boundary component.
 * \note  Each resulting edge has the surface on its right and is hence
 *        ready for a walk on with the help of BeginGeomLnext().
 * \note  The size() of the resulting list is the number of boundary
 *        components.
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshBoundaryEdgesMeshFunction:
  public FunctionBase< TMesh, typename TMesh::EdgeListPointerType >
{
public:
  // Standard types
  typedef QuadEdgeMeshBoundaryEdgesMeshFunction Self;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;
  typedef FunctionBase< TMesh,
                        typename  TMesh::EdgeListPointerType > Superclass;

  // Types in superclass:
  typedef typename Superclass::InputType  InputType;
  typedef typename Superclass::OutputType OutputType;

  // Local aliases
  typedef InputType                       MeshType;
  typedef typename MeshType::QEPrimal     QEPrimal;
  typedef typename MeshType::EdgeCellType EdgeCellType;
  typedef typename MeshType::EdgeListType EdgeListType;

  itkTypeMacro(QuadEdgeMeshBoundaryEdgesMeshFunction, FunctionBase);
  itkNewMacro(Self);

  virtual OutputType Evaluate(const InputType & mesh) const ITK_OVERRIDE;

protected:
  QuadEdgeMeshBoundaryEdgesMeshFunction() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshBoundaryEdgesMeshFunction);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.hxx"
#endif

#endif
