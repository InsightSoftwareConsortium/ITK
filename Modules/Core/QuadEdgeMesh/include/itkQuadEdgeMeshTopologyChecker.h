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
#ifndef itkQuadEdgeMeshTopologyChecker_h
#define itkQuadEdgeMeshTopologyChecker_h

#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"

namespace itk
{
/** \class QuadEdgeMeshTopologyChecker
 *  \brief Make some basic checks in order to verify that the considered
 *         mesh is not degenerated and correctly represents a surface
 *         with a potential boundary.
 *
 * We check that they are no isolated vertices, no isolated edges and
 * that the Euler formula is possible.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshTopologyChecker:public Object
{
public:
  // Standard types
  typedef QuadEdgeMeshTopologyChecker Self;
  typedef Object                      Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  typedef TMesh                                             MeshType;
  typedef typename MeshType::QEPrimal                       QEPrimal;
  typedef typename MeshType::EdgeCellType                   EdgeCellType;
  typedef typename MeshType::CellsContainerConstIterator    CellsContainerConstIterator;
  typedef QuadEdgeMeshBoundaryEdgesMeshFunction< MeshType > BoundaryEdges;

  typedef typename MeshType::PointIdentifier                PointIdentifier;
  typedef typename MeshType::CellIdentifier                 CellIdentifier;

public:
  itkNewMacro(Self);
  itkTypeMacro(QuadEdgeMeshTopologyChecker, Object);

  itkSetConstObjectMacro(Mesh, MeshType);

  typedef ::itk::IdentifierType     IdentifierType;
  typedef ::itk::OffsetValueType    OffsetValueType;

  itkSetMacro(ExpectedNumberOfPoints, PointIdentifier);
  itkSetMacro(ExpectedNumberOfEdges, CellIdentifier);
  itkSetMacro(ExpectedNumberOfFaces, CellIdentifier);
  itkSetMacro(ExpectedNumberOfBoundaries, CellIdentifier);
  itkSetMacro(ExpectedGenus, OffsetValueType);

  bool ValidateEulerCharacteristic() const;

protected:
  QuadEdgeMeshTopologyChecker();
  ~QuadEdgeMeshTopologyChecker() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshTopologyChecker);

  typedef typename MeshType::ConstPointer MeshPointer;

  MeshPointer m_Mesh;

  PointIdentifier  m_ExpectedNumberOfPoints;
  CellIdentifier   m_ExpectedNumberOfEdges;
  CellIdentifier   m_ExpectedNumberOfFaces;
  CellIdentifier   m_ExpectedNumberOfBoundaries;
  OffsetValueType  m_ExpectedGenus;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshTopologyChecker.hxx"
#endif

#endif
