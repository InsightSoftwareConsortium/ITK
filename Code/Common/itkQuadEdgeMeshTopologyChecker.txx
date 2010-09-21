/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshTopologyChecker.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshTopologyChecker_txx
#define __itkQuadEdgeMeshTopologyChecker_txx

#include "itkQuadEdgeMeshTopologyChecker.h"

namespace itk
{
template< class TMesh >
QuadEdgeMeshTopologyChecker< TMesh >
::QuadEdgeMeshTopologyChecker()
{
  m_ExpectedNumberOfPoints = 0;
  m_ExpectedNumberOfEdges = 0;
  m_ExpectedNumberOfFaces = 0;
  m_ExpectedNumberOfBoundaries = 0;
  m_ExpectedGenus = 0;
  m_Mesh = 0;
}

template< class TMesh >
bool
QuadEdgeMeshTopologyChecker< TMesh >
::ValidateEulerCharacteristic() const
{
  if ( !this->m_Mesh )
    {
    return ( false );
    }

  typename BoundaryEdges::Pointer boundaryEdges = BoundaryEdges::New();

  // Number of USED points
  unsigned long numPoints = m_Mesh->ComputeNumberOfPoints();
  // Number of USED edges
  unsigned long numEdges  = m_Mesh->ComputeNumberOfEdges();
  // Number of USED faces
  unsigned long numFaces  = m_Mesh->ComputeNumberOfFaces();
  // Number of Boundaries
  typename BoundaryEdges::OutputType
  listOfBoundaries = boundaryEdges->Evaluate( ( *m_Mesh ) );
  unsigned long numBounds = listOfBoundaries->size();
  delete listOfBoundaries;

  /**
   * Number of points
   *
   * There are two methods to get the number of points.
   * 1. itk::QuadEdgeMesh::ComputeNumberOfPoints()
   * 2. itk::Mesh::GetNumberOfPoints()
   *
   * As an itk::QuadEdgeMesh is an itk::Mesh by inheritance, the user
   * can use both. 1. will returned the number of points actually
   * used by at least one edge, while 2. will give you the number
   * of points in the container. Number of unused points can be found
   * by making the difference between the two values.
   */
  if ( m_Mesh->GetNumberOfPoints() != numPoints )
    {
    // They are isolated vertices:
    return ( false );
    }

  // The euler formula states:
  // numFaces - numEdges + numPoints == 2 - 2 * genus - numBounds
  // hence ( 2 - numBounds - numFaces + numEdges - numPoints ) must
  // be an odd number. Let's check it out:
  // Note that genus can take a negative value...
  long twiceGenus = 2 - numBounds - numFaces + numEdges - numPoints;

  if ( twiceGenus % 2 )
    {
    return ( false );
    }

  // Look is they are isolated edges
  CellsContainerConstIterator cellIterator = m_Mesh->GetCells()->Begin();
  CellsContainerConstIterator cellEnd      = m_Mesh->GetCells()->End();
  while ( cellIterator != cellEnd )
    {
    // Is the cell an Edge ?
    if ( EdgeCellType * cell =
           dynamic_cast< EdgeCellType * >( cellIterator.Value() ) )
      {
      if ( QEPrimal * edge = cell->GetQEGeom() )
        {
        // Is the edge without associated faces ?.
        if ( edge->IsWire() )
          {
          // Is it an isolated edge ?
          if ( edge->IsIsolated() && edge->GetSym()->IsIsolated() )
            {
            return ( false );
            }
          }
        }
      else // cell->GetQEGEom( ) == NULL
        {
        // supposely impossible, throw exception
        }
      }

    ++cellIterator;
    } // endof while

  return ( true );
}

template< class TMesh >
void
QuadEdgeMeshTopologyChecker< TMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ExpectedNumberOfPoints: "
     << static_cast< long >( m_ExpectedNumberOfPoints ) << std::endl;
  os << indent << "ExpectedNumberOfEdges: "
     << static_cast< long >( m_ExpectedNumberOfEdges ) << std::endl;
  os << indent << "ExpectedNumberOfFaces: "
     << static_cast< long >( m_ExpectedNumberOfFaces ) << std::endl;
  os << indent << "ExpectedNumberOfBoundaries: "
     << static_cast< long >( m_ExpectedNumberOfBoundaries ) << std::endl;
  os << indent << "ExpectedGenus: "
     << static_cast< long >( m_ExpectedGenus ) << std::endl;
  os << indent << "Mesh: " << m_Mesh << std::endl;
}
}
#endif
