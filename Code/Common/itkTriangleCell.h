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

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"

ITK_NAMESPACE_BEGIN

/** \class TriangleCell
 * TriangleCell represents a triangle for a Mesh.
 *
 * The CellBoundary wrapper for this cell is TriangleBoundary.
 *
 * Template parameters for TriangleCell:
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
class TriangleCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TriangleCell        Self;
  
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
   * The type of boundary for this triangle's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellType >  Vertex;

  /**
   * The type of boundary for this triangle's edges.
   */
  typedef LineBoundary< TPixelType , TCellType >    Edge;
  
  /** \enum
   * Triangle-specific topology numbers.
   */
  enum { NumberOfPoints   = 3,
         NumberOfVertices = 3,
         NumberOfEdges    = 3,
         CellDimension    = 2 };
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Implement the standard CellInterface.
   */
  virtual Cell::Pointer MakeCopy(void);
  virtual int GetDimension(void);
  virtual int GetNumberOfPoints(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first,
			   PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 

  /**
   * Triangle-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual Vertex::Pointer  GetVertex(CellFeatureIdentifier);
  virtual Edge::Pointer    GetEdge(CellFeatureIdentifier);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(TriangleCell, CellInterface);
  
  /**
   * Visitor interface
   */
  itkCellVisitMacro(TRIANGLE_CELL);
protected:
  /**
   * Store the number of points needed for a triangle.
   */
  PointIdentifier m_PointIds[NumberOfPoints];

  /**
   * Triangle topology data.
   */
  static const int m_Edges[3][2];
};


/** \class TriangleBoundary
 * Create a boundary-wrapped version of the TriangleCell.
 */
template <typename TPixelType, typename TCellType>
class TriangleBoundary:
  public CellBoundary< TriangleCell< TPixelType , TCellType > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TriangleBoundary    Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(TriangleBoundary, CellBoundary);
};

ITK_NAMESPACE_END

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleCell.txx"
#endif

#endif
