/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterface.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCellInterface_h
#define __itkCellInterface_h

#include "itkLightObject.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class CellInterface
 * Define an abstract interface for cells.  Actual cell types derive from
 * this class.
 *
 * Extra information in cells that are actually boundaries between other
 * cells is provided in the CellBoundary wrapper.
 *
 * Template parameters for Cell:
 *
 * TPixelType = The type stored with an entity (cell, point, or boundary).
 *
 * TCellType = Type information for cell.
 */
  
template <
  typename TPixelType,
  typename TCellType
  >
class CellInterface: public LightObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CellInterface       Self;
  
  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Save the PixelType template parameter.
   */
  typedef TPixelType                                PixelType;
  
  /**
   * Save the CellType template parameter.
   */
  typedef TCellType                                 CellType;

  /** \typedef
   * Save type information for this cell.
   */
  typedef typename CellType::CoordRep               CoordRep;
  typedef typename CellType::PointIdentifier        PointIdentifier;
  typedef typename CellType::CellIdentifier         CellIdentifier;
  typedef typename CellType::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef typename CellType::Point                  Point;
  typedef typename CellType::PointsContainer        PointsContainer;
  typedef typename CellType::UsingCellsContainer    UsingCellsContainer;
  enum { PointDimension = CellType::PointDimension };

  /** \typedef
   * An iterator through the UsingCellsContainer.
   */
  typedef typename UsingCellsContainer::iterator  UsingCellsContainerIterator;

  /**
   * Give this and all derived classes quick access to the base cell type.
   */
  typedef CellInterface< PixelType , CellType >  Cell;
  
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
   * Allow iteration over the point ID list.
   */
  typedef PointIdentifier*  PointIterator;
  
  /**
   * Allow const iteration over the point ID list.
   */
  typedef const PointIdentifier*  PointConstIterator;
  
  /**
   * Get a begin iterator to the list of point identifiers used by the cell.
   */
  virtual PointIterator      PointIdsBegin(void)=0;

  /**
   * Get a const begin iterator to the list of point identifiers used
   * by the cell.
   */
  virtual PointConstIterator PointIdsBegin(void) const =0;

  /**
   * Get an end iterator to the list of point identifiers used by the cell.
   */
  virtual PointIterator      PointIdsEnd(void)=0;

  /**
   * Get a const end iterator to the list of point identifiers used
   * by the cell.
   */
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
   * Standard part of every itk Object.
   */
  itkTypeMacro(CellInterface, LightObject);

protected:
  /**
   * Cell internal utility routines.
   */

  /**
   * Get the geometric position of a point.
   */
//  bool GetPointPosition(PointsContainer*, int localId, Point*)=0;
};


/**
 * A simple utility class to define the cell type inside a mesh type
 * structure definition.  This just makes a copy of existing type information
 * that is needed for a cell type template parameter.
 *
 * During a mesh type definition, after the appropriate types and values
 * have been defined, just have the line:
 \verbatim
 typedef MakeCellType  CellType;
 \endverbatim
 *
 * MakeCellType is a macro front-end to automatically fill in the template
 * parameters for the CellTypeInfo structure inside a mesh type structure
 * definition.
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
#include "itkCellInterface.txx"
#endif

#endif
