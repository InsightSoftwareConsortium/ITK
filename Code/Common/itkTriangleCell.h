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
   * The type of our cell base class.
   */
  typedef itkCell< TPixelType , TMeshType >        Cell;
  
  /**
   * The type of cells for this triangle's vertices and edges.
   */
  typedef itkVertexCell< TPixelType , TMeshType >  Vertex;
  typedef itkLineCell< TPixelType , TMeshType >    Edge;
  
protected:
  /**
   * A triangle needs three points to define it.
   */
  itkTriangleCell(): Cell(3) {}  
  
public:
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual CellFeatureID GetNumberOfBoundaryEntities(void);
  virtual void SetCellPoints(PointIdentifier *ptList);

  /**
   * Triangle-specific interface.
   */
  
  virtual CellFeatureID GetNumberOfVertices(void);
  virtual CellFeatureID GetNumberOfEdges(void);

  /**
   * Get the cell vertex corresponding to the given ID.
   * The ID can range from 0 to GetNumberOfVertices()-1.
   */  
  virtual Vertex::Pointer GetCellVertex(CellFeatureID);

  /**
   * Get the cell edge corresponding to the given ID.
   * The ID can range from 0 to GetNumberOfEdges()-1.
   */  
  virtual Edge::Pointer GetCellEdge(CellFeatureID);  

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkTriangleCell, itkCell);
  //virtual const char *GetClassName() const { return "itkTriangleCell"; }
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleCell.cxx"
#endif

#endif
