/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkLineCell_h
#define __itkLineCell_h

#include "itkCell.h"
#include "itkVertexCell.h"

/**
 * itkLineCell represents a line segment for itkMesh
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
class itkLineCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkLineCell        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * The type of our cell base class.
   */
  typedef itkCell< TPixelType , TMeshType >        Cell;

  /**
   * The type of cells for this lines's vertices.
   */
  typedef itkVertexCell< TPixelType , TMeshType >  Vertex;
  
  /**
   * Line-specific topology numbers.
   */
  enum { NumberOfPoints   = 2,
         NumberOfVertices = 2 };
  
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual CellFeatureCount GetNumberOfBoundaryEntities(int dimension);
  virtual void SetCellPoints(PointIdentifier *ptList);

  /**
   * Line-specific interface.
   */
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual Vertex::Pointer GetCellVertex(CellFeatureID);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkLineCell, itkCell);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  itkLineCell(): Cell(NumberOfPoints) {}  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.cxx"
#endif

#endif
