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
  typedef itkMesh< PixelType , MeshType >     	      Mesh;
  typedef typename MeshType::PointIdentifier  	      PointIdentifier;
  typedef typename MeshType::CellIdentifier   	      CellIdentifier;
  typedef typename MeshType::CoordRep         	      CoordRep;
  typedef typename MeshType::CellFeatureIdentifier    CellFeatureIdentifier;
  enum { PointDimension = MeshType::PointDimension };
  
  /**
   * Let any derived cell type classes have easy access to their base type.
   */
  typedef itkCell Cell;
  
  /**
   * The type of point used by the cell.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;

  /**
   * A useful rename.
   */
  typedef CellFeatureIdentifier CellFeatureCount;  

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
  virtual void SetCellPoints(PointIdentifier *ptList)=0;
  
  /**
   * Set the point identifier for a given spot in the point list for the cell.
   */
  virtual void SetCellPoint(int localId, PointIdentifier ptId);

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
  Point GetPointPosition(Mesh* mesh, int localId);
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCell.cxx"
#endif

#endif
