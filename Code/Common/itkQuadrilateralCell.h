/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadrilateralCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkQuadrilateralCell_h
#define __itkQuadrilateralCell_h

#include "itkCell.h"
#include "itkLineCell.h"

/**
 * itkQuadrilateralCell represents a quadrilateral for itkMesh
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
class itkQuadrilateralCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkQuadrilateralCell   Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * The type of cells for this quadrilateral's vertices and edges.
   */
  typedef itkVertexCell< TPixelType , TMeshType >  Vertex;
  typedef itkLineCell< TPixelType , TMeshType >    Edge;
  
  /**
   * Quadrilateral-specific topology numbers.
   */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 4,
         CellDimension    = 2 };
  
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetCellPoints(PointIdentifier *ptList);

  /**
   * Quadrilateral-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);

  /**
   * Get the cell vertex corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfVertices()-1.
   */  
  virtual Vertex::Pointer GetCellVertex(CellFeatureIdentifier);

  /**
   * Get the cell edge corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfEdges()-1.
   */  
  virtual Edge::Pointer GetCellEdge(CellFeatureIdentifier);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkQuadrilateralCell, itkCell);
  
protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  itkQuadrilateralCell(): Cell(NumberOfPoints) {}

  /**
   * Quadrilateral topology data.
   */
  static const int m_Edges[4][2];
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadrilateralCell.cxx"
#endif

#endif
