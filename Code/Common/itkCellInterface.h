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

ITK_NAMESPACE_BEGIN

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
  typedef typename CellType::InterpolationWeight    InterpolationWeight;
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
  typedef CellInterface  Cell;
  
  /**
   * A useful rename.
   */
  typedef CellFeatureIdentifier  CellFeatureCount;

  /**
   * Allow iteration over the point ID list.
   */
  typedef PointIdentifier*  PointIdIterator;
  
  /**
   * Allow const iteration over the point ID list.
   */
  typedef const PointIdentifier*  PointIdConstIterator;
  
  /**
   * Public interface routines.
   */
  
  /**
   * Create a new copy of this cell.  This is provided so that a copy can
   * be made without knowing the cell type.
   */
  virtual Pointer MakeCopy(void)=0;
  
  /**
   * Get the topological dimension of this cell.
   */
  virtual int GetDimension(void)=0;

  /**
   * Get the interpolation order of the cell.  Usually linear.
   */
  virtual int GetInterpolationOrder(void);
  
  /**
   * Get the number of points required to define the cell.
   */
  virtual int GetNumberOfPoints(void)=0;
  
  /**
   * Get the number of boundary features of a given dimension on this cell.
   */
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension)=0;
  
  /**
   * Get the boundary feature corresponding to the given dimension and Id.
   */
  virtual Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier)=0;

  /**
   * Get the point id list used by the cell in a form suitable to pass to
   * SetPointIds(first) on another cell.  This is equivalent to
   * PointIdsBegin() const.
   */
  virtual PointIdConstIterator GetPointIds(void) const;
  
  /**
   * Set the point id list used by the cell.  It is assumed that the given
   * iterator can be incremented and safely de-referenced enough times to 
   * get all the point ids needed by the cell.
   */
  virtual void SetPointIds(PointIdConstIterator first)=0;
  
  /**
   * Set the point id list used by the cell.  It is assumed that the range
   * of iterators [first, last) contains the correct number of points needed to
   * define the cell.  The position *last is NOT referenced, so it can safely
   * be one beyond the end of an array or other container.
   */
  virtual void SetPointIds(PointIdConstIterator first,
			   PointIdConstIterator last)=0;
  
  /**
   * Set the point identifier for a given spot in the point list for the cell.
   */
  virtual void SetPointId(int localId, PointIdentifier)=0;
  
  /**
   * Get a begin iterator to the list of point identifiers used by the cell.
   */
  virtual PointIdIterator PointIdsBegin(void)=0;

  /**
   * Get a const begin iterator to the list of point identifiers used
   * by the cell.
   */
  virtual PointIdConstIterator PointIdsBegin(void) const =0;

  /**
   * Get an end iterator to the list of point identifiers used by the cell.
   */
  virtual PointIdIterator PointIdsEnd(void)=0;

  /**
   * Get a const end iterator to the list of point identifiers used
   * by the cell.
   */
  virtual PointIdConstIterator PointIdsEnd(void) const =0;

  /**
   * Given the parametric coordinates of a point in the cell
   * (pCoords[CellDimension]), get the closest cell boundary feature of
   * topological dimension CellDimension-1.  If the "inside" pointer is not
   * NULL, the flag is set to indicate whether the point is inside the cell.
   */
  virtual Pointer GetClosestBoundary(CoordRep pCoords[], bool* inside) {return Pointer();}

  /**
   * Given the geometric coordinates of a point (coord[PointDimension]),
   * return whether it is inside the cell.  Also perform the following
   * calculations, if the corresponding result pointers are not NULL:
   *
   *  - Find the closest point in or on the cell to the given point
   *     (Returns through pointer to array: closestPoint[PointDimension]).
   *
   *  - Get the cell's parametric coordinates for the given point
   *     (Returns through pointer to array: pCoords[CellDimension]).
   *
   *  - Get the square of the distance between the point and the cell
   *     (this is the distance from the point to the closest point,
   *      returned through "dist2" pointer).
   *
   *  - Get the interpolation weights for the cell
   *     (Returns through pointer to array: weights[NumberOfPoints]).
   */
  virtual bool EvaluatePosition(CoordRep coords[PointDimension],
				CoordRep closestPoint[PointDimension],
				CoordRep pCoords[],
				CoordRep* dist2,
				InterpolationWeight weights[]) {return bool();}
  
  /**
   * Given the parametric coordinates of a point in the cell
   * (pCoords[CellDimension]), determine its global geometric coordinates
   * (returned through pointer to array: coords[PointDimension]).
   * Also get the interpolation weights if pointer is not NULL
   * (returned through pointer to array: weights[NumberOfPoints]).
   */
  virtual void EvaluateLocation(CoordRep pCoords[],
				CoordRep coords[PointDimension],
				InterpolationWeight weights[]) {}

  /**
   * Intersect the cell with a line given by an origin (origin[PointDimension])
   * and direction (direction[PointDimension]).  The intersection point
   * found will be within the given tolerance of the real intersection.
   * Get the following results if the corresponding pointers are not NULL:
   *
   *  - The intersection point's geometric coordinates (returned through
   *     pointer to array: coords[PointDimension]).
   *
   *  - The line's parametric coordinate of the intersection point
   *     (returned through "t" pointer).
   *
   *  - The cell's parametric coordinates of the intersection point
   *     (returned through pointer to array: pCoords[CellDimension]).
   *
   * Returns whether an intersection exists within the given tolerance.
   */
  virtual bool IntersectWithLine(CoordRep origin[PointDimension],
				 CoordRep direction[PointDimension],
				 CoordRep tolerance,
				 CoordRep coords[PointDimension],
				 CoordRep* t,
				 CoordRep pCoords[]) {return bool();}
  
  /**
   * Compute cell bounding box and store in the user-provided array.
   * Array is ordered (xmin, xmax,  ymin, ymax, ....).  A pointer to the
   * array is returned for convenience.  This allows code like:
   * "CoordRep* bounds = cell->GetBoundingBox(new CoordRep[6]);".
   */
  CoordRep* GetBoundingBox(CoordRep bounds[PointDimension*2]) {return NULL;}

  /**
   * Compute the square of the diagonal length of the bounding box.
   */
  CoordRep GetBoundingBoxDiagonalLength2(void) {return CoordRep();}

  /**
   * Intersect the given bounding box (bounds[PointDimension*2]) with a line
   * given by an origin (origin[PointDimension]) and direction
   * (direction[PointDimension]). Get the following results if the
   * corresponding pointers are not NULL:
   *
   *  - The intersection point's geometric coordinates (returned through
   *     pointer to array: coords[PointDimension]).
   *
   *  - The line's parametric coordinate of the intersection point
   *     (returned through "t" pointer).
   *
   * Returns whether an intersection exists.
   */
  virtual bool IntersectBoundingBoxWithLine(CoordRep bounds[PointDimension*2],
					    CoordRep origin[PointDimension],
					    CoordRep direction[PointDimension],
					    CoordRep coords[PointDimension],
					    CoordRep* t) {return bool();}
  
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
template <int VPointDimension, typename TCoordRep,
  typename TInterpolationWeight, typename TPointIdentifier,
  typename TCellIdentifier, typename TCellFeatureIdentifier,
  typename TPoint, typename TPointsContainer,
  typename TUsingCellsContainer>
class CellTypeInfo
{
public:
  enum { PointDimension = VPointDimension };
  typedef TCoordRep               CoordRep;
  typedef TInterpolationWeight    InterpolationWeight;
  typedef TPointIdentifier  	  PointIdentifier;
  typedef TCellIdentifier   	  CellIdentifier;
  typedef TCellFeatureIdentifier  CellFeatureIdentifier;
  typedef TPoint                  Point;
  typedef TPointsContainer        PointsContainer;
  typedef TUsingCellsContainer    UsingCellsContainer;
};

#define MakeCellType \
  CellTypeInfo<PointDimension, CoordRep, InterpolationWeight,  \
               PointIdentifier, CellIdentifier, CellFeatureIdentifier, \
               Point, PointsContainer, UsingCellsContainer>

ITK_NAMESPACE_END

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellInterface.txx"
#endif

#endif
