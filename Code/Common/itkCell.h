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
#ifndef __itkCell_h
#define __itkCell_h

#include <vector>
#include <set>

#include "itkObject.h"
#include "itkSmartPointer.h"
#include "itkMeshType.h"
#include "itkContainerInterfaces.h"
#include "itkVectorContainer.h"
#include "itkSetContainer.h"

/**
 * itkCell defines the data that are stored in a Cell.  It is derived from
 * itkCellBase, but is dependent on mesh type.  Cell types are defined
 * by deriving from itkCell<>.
 */

/**
 * Forward declare itkMesh so itkCell can reference it.
 */
template <typename TPixelType, typename TMeshType>  class itkMesh;

template <
  /**
   * The type associated with a point, cell, or boundary for use in storing
   * its data.
   */
  typename TPixelType,

  /**
   * Type information of mesh containing cell.
   */
  typename TMeshType = itkMeshTypeDefault
  >
class itkCell: public itkObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkCell                Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * Extract some type information from the parent mesh's type.
   */
  typedef TPixelType  PixelType;
  typedef TMeshType   MeshType;
  typedef itkMesh< PixelType , MeshType >     Mesh;
  typedef typename MeshType::PointIdentifier  PointIdentifier;
  typedef typename MeshType::CellIdentifier   CellIdentifier;
  typedef typename MeshType::CoordRep         CoordRep;
  typedef typename MeshType::CellFeatureID    CellFeatureID;
  enum { PointDimension = MeshType::PointDimension };
  
  /**
   * The type of point used by the cell.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;

  /**
   * A useful rename.
   */
  typedef CellFeatureID CellFeatureCount;  

  /**
   * Public interface routines.
   */
  virtual CellFeatureCount GetNumberOfBoundaryEntities(int dimension)=0;
  virtual void SetCellPoints(PointIdentifier *ptList)=0;
  virtual void SetCellPoint(CellFeatureID, PointIdentifier ptId);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkCell, itkObject);

protected:
  /**
   * The vector of point identifiers of points used by this cell.
   * Specific cell types know how to interpret the ordering of these
   * identifiers.
   */
  typedef std::vector< PointIdentifier >  PointIdentifierContainer;
  PointIdentifierContainer  m_PointIds;
  
  /**
   * The set of cells that have been assigned to use this cell as a boundary.
   * Top-level cells in the mesh always have an empty set of parent cells.
   */
  typedef std::set< CellIdentifier >  ParentCellContainer;
  ParentCellContainer  m_ParentCells;
  
  /**
   * Cell internal utility routines.
   */

  /**
   * The constructor allocates the point identifier container.
   */
  itkCell() {}
  itkCell(unsigned long numPoints): m_PointIds(numPoints) {}  
  
  /**
   * Get the geometrical position of a point.
   */
  Point GetPointPosition(Mesh* mesh, int localID);
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCell.cxx"
#endif

#endif
