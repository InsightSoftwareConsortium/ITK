/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTetrahedronCell_h
#define __itkTetrahedronCell_h

#include "itkCell.h"
#include "itkTriangleCell.h"

/**
 * itkTetrahedronCell represents a tetrahedron for itkMesh
 */

template <
  /**
   * The type associated with a point, cell, or boundary for use in storing
   * its data.
   */
  typename TPixelType,

  /**
   * Type information of mesh containing cell.
   */
  typename TMeshType = itkMeshTypeDefault
  >
class itkTetrahedronCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkTetrahedronCell        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * The type of cells for this tetrahedron's vertices, edges, and faces.
   */
  typedef itkVertexCell< TPixelType , TMeshType >    Vertex;
  typedef itkLineCell< TPixelType , TMeshType >      Edge;
  typedef itkTriangleCell< TPixelType , TMeshType >  Face;
  
  /**
   * Tetrahedron-specific topology numbers.
   */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 6,
         NumberOfFaces    = 4,
         CellDimension    = 3 };

  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureId, Mesh*);
  virtual void SetCellPoints(PointIdentifier *ptList);

  /**
   * Tetrahedron-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual CellFeatureCount GetNumberOfFaces(void);

  /**
   * Get the cell vertex corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfVertices()-1.
   */  
  virtual Vertex::Pointer GetCellVertex(CellFeatureId, Mesh*);

  /**
   * Get the cell edge corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfEdges()-1.
   */  
  virtual Edge::Pointer GetCellEdge(CellFeatureId, Mesh*);  

  /**
   * Get the cell face corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfFaces()-1.
   */  
  virtual Face::Pointer GetCellFace(CellFeatureId, Mesh*);  

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkTetrahedronCell, itkCell);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  itkTetrahedronCell(): Cell(NumberOfPoints) {}  
  
  /**
   * Tetrahedron topology data.
   */
  static const int m_Edges[6][2];
  static const int m_Faces[4][3];
};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTetrahedronCell.cxx"
#endif

#endif
