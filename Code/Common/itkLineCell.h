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
#include "itkCellBoundary.h"
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
  typename TCellType
  >
class itkLineCell: public itkCell< TPixelType , TCellType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkLineCell        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * The type of cells for this lines's vertices.
   */
  typedef itkVertexBoundary< TPixelType , TCellType >  Vertex;
  
  /**
   * Line-specific topology numbers.
   */
  enum { NumberOfPoints   = 2,
         NumberOfVertices = 2,
         CellDimension    = 1 };
  
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetCellPoints(const PointIdentifier *ptList);
  virtual void SetCellPoints(const PointIdentifier* first,
			     const PointIdentifier* last);
  virtual void SetCellPoint(int localId, PointIdentifier);
  virtual PointIterator      PointIdsBegin(void);
  virtual PointConstIterator PointIdsBegin(void) const;
  virtual PointIterator      PointIdsEnd(void);
  virtual PointConstIterator PointIdsEnd(void) const; 

  /**
   * Line-specific interface.
   */
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual Vertex::Pointer GetCellVertex(CellFeatureIdentifier);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkLineCell, itkCell);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
};


/**
 * Create the boundary-wrapped version of this cell type.
 */
template <typename TPixelType, typename TCellType>
class itkLineBoundary:
  public itkCellBoundary< itkLineCell< TPixelType , TCellType > >
{
public:
  typedef itkLineBoundary        Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  static Pointer New(void);
  
  itkTypeMacro(itkLineBoundary, itkCellBoundary);
};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.cxx"
#endif

#endif
