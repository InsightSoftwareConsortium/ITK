/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkCell ....
 */

#ifndef __itkCell_h
#define __itkCell_h

#include "itkSmartPointer.h"

namespace itk
{

/**
 * Template parameters for Cell:
 *
 * TPixelType = 
 *     The type stored with an entity (cell, point, or boundary).
 * TCellType = 
 *     Type information for cell.
 */
  
template <
  typename TPixelType,
  typename TCellType
  >
class Cell
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef Cell                   Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * Save type information for this cell.
   */
  typedef TPixelType                                PixelType;
  typedef TCellType                                 CellType;
  typedef typename CellType::CoordRep               CoordRep;
  typedef typename CellType::PointIdentifier        PointIdentifier;
  typedef typename CellType::CellIdentifier         CellIdentifier;
  typedef typename CellType::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef typename CellType::Point                  Point;
  typedef typename CellType::PointsContainer        PointsContainer;
  typedef typename CellType::UsingCellsContainer    UsingCellsContainer;
  enum { PointDimension = CellType::PointDimension };

  typedef typename UsingCellsContainer::iterator  UsingCellsContainerIterator;
  
  /**
   * A useful rename.
   */
  typedef CellFeatureIdentifier  CellFeatureCount;
  
  /**
   * Public interface routines.
   */
  
  /**
   * Get the topological dimension of this cell.
   */
  virtual int GetCellDimension(void)=0;
  
  /**
   * Get the number of boundary features of a given dimension on this cell.
   */
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension)=0;
  
  /**
   * Get the boundary feature corresponding to the given dimension and Id.
   */
  virtual Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier)=0;
  
  /**
   * Set the point list used by the cell.  It is assumed that the argument
   * ptList points to an array of PointIdentifier values of length equal to
   * the number of points needed to define the cell.
   */
  virtual void SetCellPoints(const PointIdentifier *ptList)=0;
  
  /**
   * Set the point list used by the cell.  It is assumed that the range
   * of iterators [first, last) contains the correct number of points needed to
   * define the cell.  The position *last is NOT referenced, so it can safely
   * be one beyond the end of an array.
   */
  virtual void SetCellPoints(const PointIdentifier* first,
			     const PointIdentifier* last)=0;
  
  /**
   * Set the point identifier for a given spot in the point list for the cell.
   */
  virtual void SetCellPoint(int localId, PointIdentifier)=0;
  
  /**
   * Provide an interface to allow iteration over the point ID list.
   */
  typedef PointIdentifier*  PointIterator;
  typedef const PointIdentifier*  PointConstIterator;
  virtual PointIterator      PointIdsBegin(void)=0;
  virtual PointConstIterator PointIdsBegin(void) const =0;
  virtual PointIterator      PointIdsEnd(void)=0;
  virtual PointConstIterator PointIdsEnd(void) const =0;
  
  /**
   * Interface to the boundary form of the cell to set/get UsingCells.
   * See the boundary wrapper source for more information.
   */
  virtual bool IsBoundary(void);
  virtual void AddUsingCell(CellIdentifier);
  virtual void RemoveUsingCell(CellIdentifier);
  virtual bool IsUsingCell(CellIdentifier);
  virtual int GetNumUsingCells(void);
  virtual UsingCellsContainerIterator UsingCellsBegin(void);
  virtual UsingCellsContainerIterator UsingCellsEnd(void);
  
  /**
   * ITK standard routines.
   */
  itkTypeMacro(Cell,none);
  void Register(void);
  void UnRegister(void);

protected:
  /**
   * Cell internal utility routines.
   */

  /**
   * Constructor to initialize internal data.
   */
  Cell();
  
  /**
   * Get the geometric position of a point.
   */
//  bool GetPointPosition(PointsContainer*, int localId, Point*)=0;
  
private:
  int m_ReferenceCount;     // Number of uses of this object by other objects.
};


/**
 * Define a simple utility to define the cell type inside a mesh type
 * structure definition.  This just makes a copy of existing type information
 * that is needed for a cell type template parameter.
 *
 * During a mesh type definition, after the appropriate types and values
 * have been defined, just have the line:
 *   typedef MakeCellType  CellType;
 */
template <int VPointDimension,typename TCoordRep,
  typename TPointIdentifier,typename TCellIdentifier,
  typename TCellFeatureIdentifier, typename TPoint, 
  typename TPointsContainer, typename TUsingCellsContainer>
class CellTypeInfo
{
public:
  enum { PointDimension = VPointDimension };
  typedef TCoordRep               CoordRep;
  typedef TPointIdentifier  	  PointIdentifier;
  typedef TCellIdentifier   	  CellIdentifier;
  typedef TCellFeatureIdentifier  CellFeatureIdentifier;
  typedef TPoint                  Point;
  typedef TPointsContainer        PointsContainer;
  typedef TUsingCellsContainer    UsingCellsContainer;
};

#define MakeCellType \
  CellTypeInfo<PointDimension, CoordRep, PointIdentifier, \
               CellIdentifier, CellFeatureIdentifier, Point, \
               PointsContainer, UsingCellsContainer>

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCell.cxx"
#endif

#endif
