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

namespace itk
{

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
 * TCellTraits =
 *     Type information of mesh containing cell.
 */

template <
  typename TPixelType,
  typename TCellTraits
  >
class TriangleCell: public CellInterface< TPixelType , TCellTraits >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TriangleCell        Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef CellInterface<TPixelType,TCellTraits>  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Save the PixelType template parameter.
   */
  typedef TPixelType                                PixelType;
  
  /**
   * Save the CellTraits template parameter.
   */
  typedef TCellTraits                                 CellTraits;

  /**
   * Pick-up typedefs from superclass
   */
  typedef typename CellTraits::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdConstIterator
                   PointIdConstIterator;

  /**
   * Save some template parameter information.
   */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellTraits::PointDimension };
  typedef typename CellInterface<TPixelType,TCellTraits>::Pointer CellPointer;

  /**
   * The type of boundary for this triangle's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellTraits >  Vertex;
  typedef typename Vertex::Pointer VertexPointer;

  /**
   * The type of boundary for this triangle's edges.
   */
  typedef LineBoundary< TPixelType , TCellTraits >    Edge;
  typedef typename Edge::Pointer EdgePointer;
  
  /**
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
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void);
  virtual int GetNumberOfPoints(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
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
  virtual VertexPointer  GetVertex(CellFeatureIdentifier);
  virtual EdgePointer    GetEdge(CellFeatureIdentifier);

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
template <typename TPixelType, typename TCellTraits>
class TriangleBoundary:
  public CellBoundary< TriangleCell< TPixelType , TCellTraits > >
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

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleCell.txx"
#endif

#endif
