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

#include "itkCell.h"
#include "itkCellBoundary.h"

/**
 * itkVertexCell represents a single vertex for itkMesh.
 */

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
class itkVertexCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkVertexCell        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * Save some template parameter information.
   */
  typedef typename MeshType::CoordRep         CoordRep;
  typedef typename MeshType::PointIdentifier  PointIdentifier;
  enum { PointDimension = MeshType::PointDimension };

  /**
   * The vertex has only a point as its boundary.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;

  /**
   * Vertex-specific topology numbers.
   */
  enum { NumberOfPoints = 1,
         CellDimension  = 0 };
  
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetCellPoints(const PointIdentifier *ptList);
  virtual void SetCellPoints(const PointIdentifier* first,
			     const PointIdentifier* last);
  virtual void SetCellPoint(int localId, PointIdentifier);
  
  /**
   * Vertex-specific interface.
   */
  virtual PointIdentifier GetCellPoint(void);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkVertexCell, itkCell);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
};


/**
 * Create the boundary-wrapped version of this cell type.
 */
template <typename TPixelType, typename TMeshType = itkMeshTypeDefault>
class itkVertexBoundary:
  public itkCellBoundary< itkVertexCell< TPixelType , TMeshType > >
{};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVertexCell.cxx"
#endif

#endif
