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

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkVertexCell.h"

namespace itk
{

/** \class LineCell
 * LineCell represents a line segment for a Mesh.
 *
 * The CellBoundary wrapper for this cell is LineBoundary.
 *
 * Template parameters for LineCell:
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
class LineCell: public CellInterface< TPixelType , TCellTraits >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LineCell            Self;
  
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
   * The type of boundary for this lines's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellTraits >  Vertex;
  typedef typename Vertex::Pointer VertexPointer;
  
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
   * Line-specific interface.
   */
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual VertexPointer GetVertex(CellFeatureIdentifier);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(LineCell, CellInterface);

  /**
   * Visitor interface
   */
  itkCellVisitMacro(LINE_CELL);

protected:
  /**
   * Store number of points needed for a line segment.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
};


/** \class LineBoundary
 * Create a boundary-wrapped version of the LineCell.
 */
template <typename TPixelType, typename TCellTraits>
class LineBoundary:
  public CellBoundary< LineCell< TPixelType , TCellTraits > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LineBoundary        Self;

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
  itkTypeMacro(LineBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.txx"
#endif

#endif
