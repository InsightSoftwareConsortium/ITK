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

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"

namespace itk
{

/** \class QuadrilateralCell
 * QuadrilateralCell represents a quadrilateral for a Mesh.
 *
 * The CellBoundary wrapper for this cell is QuadrilateralBoundary.
 *
 * Template parameters for QuadrilateralCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 *
 * TCellType =
 *     Type information of mesh containing cell.
 */

template <
  typename TPixelType,
  typename TCellType
  >
class QuadrilateralCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef QuadrilateralCell   Self;
  
  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /** \typedef
   * Save some template parameter information.
   */
  typedef typename CellType::CoordRep         CoordRep;
  typedef typename CellType::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellType::PointDimension };

  /**
   * The type of boundary for this quadrilateral's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellType >  Vertex;

  /**
   * The type of boundary for this quadrilateral's edges.
   */
  typedef LineBoundary< TPixelType , TCellType >    Edge;
  
  /** \enum
   * Quadrilateral-specific topology numbers.
   */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 4,
         CellDimension    = 2 };
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Implement the standard CellInterface.
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
   * Quadrilateral-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual Vertex::Pointer GetCellVertex(CellFeatureIdentifier);
  virtual Edge::Pointer GetCellEdge(CellFeatureIdentifier);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(QuadrilateralCell, CellInterface);
  
protected:
  /**
   * Store the number of points needed for a quadrilateral.
   */
  PointIdentifier m_PointIds[NumberOfPoints];

  /**
   * Quadrilateral topology data.
   */
  static const int m_Edges[4][2];
};


/** \class QuadrilateralBoundary
 * Create a boundary-wrapped version of the QuadrilateralCell.
 */
template <typename TPixelType, typename TCellType>
class QuadrilateralBoundary:
  public CellBoundary< QuadrilateralCell< TPixelType , TCellType > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef QuadrilateralBoundary  Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>     Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(QuadrilateralBoundary, CellBoundary);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadrilateralCell.txx"
#endif

#endif
