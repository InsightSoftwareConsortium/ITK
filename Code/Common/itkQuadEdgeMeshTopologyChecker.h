/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshTopologyChecker.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshTopologyChecker_h
#define __itkQuadEdgeMeshTopologyChecker_h

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
 * http://insight-journal.org/midas/handle.php?handle=1926/306
 *
 */
template< class TMesh >
class ITK_EXPORT QuadEdgeMeshTopologyChecker:public Object
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
public:
  itkNewMacro(Self);
  itkTypeMacro(QuadEdgeMeshTopologyChecker, Object);

  itkSetConstObjectMacro(Mesh, MeshType);

  // FIXME this probably should be taken from the traits of the Mesh
  typedef unsigned long IdentifierType;

  itkSetMacro(ExpectedNumberOfPoints, IdentifierType);
  itkSetMacro(ExpectedNumberOfEdges, IdentifierType);
  itkSetMacro(ExpectedNumberOfFaces, IdentifierType);
  itkSetMacro(ExpectedNumberOfBoundaries, IdentifierType);
  itkSetMacro(ExpectedGenus, IdentifierType);

  bool ValidateEulerCharacteristic() const;

protected:
  QuadEdgeMeshTopologyChecker();
  ~QuadEdgeMeshTopologyChecker(){}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  QuadEdgeMeshTopologyChecker(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented

  typedef typename MeshType::ConstPointer MeshPointer;

  MeshPointer m_Mesh;

  IdentifierType m_ExpectedNumberOfPoints;
  IdentifierType m_ExpectedNumberOfEdges;
  IdentifierType m_ExpectedNumberOfFaces;
  IdentifierType m_ExpectedNumberOfBoundaries;
  IdentifierType m_ExpectedGenus;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshTopologyChecker.txx"
#endif

#endif
