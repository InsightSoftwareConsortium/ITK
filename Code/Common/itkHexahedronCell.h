/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHexahedronCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkHexahedronCell_h
#define __itkHexahedronCell_h

#include "itkCell.h"
#include "itkCellBoundary.h"
#include "itkQuadrilateralCell.h"

/**
 * itkHexahedronCell represents a hexahedron for itkMesh
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
class itkHexahedronCell: public itkCell< TPixelType , TMeshType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkHexahedronCell        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * The type of cells for this hexahedron's vertices, edges, and faces.
   */
  typedef itkVertexBoundary< TPixelType , TMeshType >         Vertex;
  typedef itkLineBoundary< TPixelType , TMeshType >           Edge;
  typedef itkQuadrilateralBoundary< TPixelType , TMeshType >  Face;
  
  /**
   * Hexahedron-specific topology numbers.
   */
  enum { NumberOfPoints   =  8,
         NumberOfVertices =  8,
         NumberOfEdges    = 12,
         NumberOfFaces    =  6,
         CellDimension    =  3 };

  /**
   * Implement the standard cell API.
   */
  static Pointer New(void);
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetCellPoints(const PointIdentifier *ptList);

  /**
   * Hexahedron-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual CellFeatureCount GetNumberOfFaces(void);

  /**
   * Get the cell vertex corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfVertices()-1.
   */  
  virtual Vertex::Pointer GetCellVertex(CellFeatureIdentifier);

  /**
   * Get the cell edge corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfEdges()-1.
   */  
  virtual Edge::Pointer GetCellEdge(CellFeatureIdentifier);  

  /**
   * Get the cell face corresponding to the given Id.
   * The Id can range from 0 to GetNumberOfFaces()-1.
   */  
  virtual Face::Pointer GetCellFace(CellFeatureIdentifier);  

  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkHexahedronCell, itkCell);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  itkHexahedronCell(): Cell(NumberOfPoints) {}  
  
  /**
   * Hexahedron topology data.
   */
  static const int m_Edges[12][2];
  static const int m_Faces[6][4];
};


/**
 * Create the boundary-wrapped version of this cell type.
 */
template <typename TPixelType, typename TMeshType = itkMeshTypeDefault>
class itkHexahedronBoundary:
  public itkCellBoundary< itkHexahedronCell< TPixelType , TMeshType > >
{};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHexahedronCell.cxx"
#endif

#endif
