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
/**
 * LineCell represents a line segment for Mesh
 */

#ifndef __itkLineCell_h
#define __itkLineCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkVertexCell.h"

namespace itk
{

/**
 * Template parameters for LineCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 * TCellType =
 *     Type information of mesh containing cell.
 */

template <
  typename TPixelType,
  typename TCellType
  >
class LineCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef LineCell               Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * Save some template parameter information.
   */
  typedef typename CellType::CoordRep         CoordRep;
  typedef typename CellType::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellType::PointDimension };
  
  /**
   * The type of cells for this lines's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellType >  Vertex;
  
  /**
   * Line-specific topology numbers.
   */
  enum { NumberOfPoints   = 2,
         NumberOfVertices = 2,
         CellDimension    = 1 };
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Implement the standard cell API.
   */
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
  itkTypeMacro(LineCell, CellInterface);

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
class LineBoundary:
  public CellBoundary< LineCell< TPixelType , TCellType > >
{
public:
  typedef LineBoundary           Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  static Pointer New(void);
  
  itkTypeMacro(LineBoundary, CellBoundary);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.cxx"
#endif

#endif
