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
   * The type of our cell base class.
   */
  typedef itkCell< TPixelType , TMeshType >        Cell;
  
  /**
   * The vertex has only a point as its boundary.
   */
  typedef itkPoint< PointDimension , CoordRep >  Point;
  
protected:
  /**
   * A vertex needs only one point to define it.
   */
  itkVertexCell(): Cell(1) {}
  
public:
  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual CellFeatureID GetNumberOfBoundaryEntities(void);
  virtual void SetCellPoints(PointIdentifier *ptList);
  
  /**
   * Vertex-specific interface.
   */
  virtual PointIdentifier GetCellPoint(void);

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  virtual const char *GetClassName() const { return "itkVertexCell"; }
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVertexCell.cxx"
#endif

#endif
