/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVertexCell_h
#define __itkVertexCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"

ITK_NAMESPACE_BEGIN

/** \class VertexCell
 * VertexCell represents a single vertex for a Mesh.
 *
 * The CellBoundary wrapper for this cell is VertexBoundary.
 *
 * Template parameters for VertexCell:
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
class VertexCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VertexCell          Self;
  
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

  /** \enum
   * Vertex-specific topology numbers.
   */
  enum { NumberOfPoints = 1,
         CellDimension  = 0 };
  
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
   * Vertex-specific interface.
   */
  virtual void SetPointId(PointIdentifier);
  virtual PointIdentifier GetPointId(void);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(VertexCell, CellInterface);

protected:
  /**
   * Store the number of points needed for a vertex.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
};


/** \class VertexBoundary
 * Create a boundary-wrapped version of the VertexCell.
 */
template <typename TPixelType, typename TCellType>
class VertexBoundary:
  public CellBoundary< VertexCell< TPixelType , TCellType > >
{
public:
  /**
   * Standard "Self" typdef.
   */
  typedef VertexBoundary      Self;
  
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
  itkTypeMacro(VertexBoundary, CellBoundary);
};

ITK_NAMESPACE_END

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVertexCell.txx"
#endif

#endif
