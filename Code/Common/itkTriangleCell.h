/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTriangleCell_h
#define __itkTriangleCell_h

#include "itkCell.h"
#include "itkLineCell.h"

/**
 * itkTriangleCell represents a triangle for itkMesh
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
class itkTriangleCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkTriangleCell        Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * The type of cells for this triangle's vertices and edges.
   */
  typedef itkVertexCell< TPixelType , TMeshType >  Vertex;
  typedef itkLineCell< TPixelType , TMeshType >    Edge;
  
  /**
   * Triangle-specific topology numbers.
   */
  enum { NumberOfPoints   = 3,
         NumberOfVertices = 3,
         NumberOfEdges    = 3,
         CellDimension    = 2 };
  
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureId);
  virtual void SetCellPoints(PointIdentifier *ptList);

  /**
   * Triangle-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);

  /**
   * Get the cell vertex corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfVertices()-1.
   */  
  virtual Vertex::Pointer GetCellVertex(CellFeatureId);

  /**
   * Get the cell edge corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfEdges()-1.
   */  
  virtual Edge::Pointer GetCellEdge(CellFeatureId);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkTriangleCell, itkCell);
  
protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  itkTriangleCell(): Cell(NumberOfPoints) {}

  /**
   * Triangle topology data.
   */
  static const int m_Edges[3][2];
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleCell.cxx"
#endif

#endif
