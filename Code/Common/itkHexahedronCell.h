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
/**
 * HexahedronCell represents a hexahedron for Mesh
 */

#ifndef __itkHexahedronCell_h
#define __itkHexahedronCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkQuadrilateralCell.h"

namespace itk
{

/**
 * Template parameters for HexahedronCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 * TCellType =
 *     Type information of mesh containing cell.
 */

template <
  typename TPixelType,
  typename TCellType
  >
class HexahedronCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef HexahedronCell      Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Save some template parameter information.
   */
  typedef typename CellType::CoordRep         CoordRep;
  typedef typename CellType::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellType::PointDimension };

  /**
   * The type of cells for this hexahedron's vertices, edges, and faces.
   */
  typedef VertexBoundary< TPixelType , TCellType >         Vertex;
  typedef LineBoundary< TPixelType , TCellType >           Edge;
  typedef QuadrilateralBoundary< TPixelType , TCellType >  Face;
  
  /**
   * Hexahedron-specific topology numbers.
   */
  enum { NumberOfPoints   =  8,
         NumberOfVertices =  8,
         NumberOfEdges    = 12,
         NumberOfFaces    =  6,
         CellDimension    =  3 };

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Implement the standard cell API.
   */
  virtual int GetCellDimension(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual Cell::Pointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetCellPoints(const PointIdentifier *ptList);
  virtual void SetCellPoints(const PointIdentifier* first,
			     const PointIdentifier* last);
  virtual void SetCellPoint(int localId, PointIdentifier);
  virtual PointIterator      PointIdsBegin(void);
  virtual PointConstIterator PointIdsBegin(void) const;
  virtual PointIterator      PointIdsEnd(void);
  virtual PointConstIterator PointIdsEnd(void) const; 

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
  itkTypeMacro(HexahedronCell, CellInterface);

protected:
  /**
   * Allocate number of points needed for this cell type.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
  
  /**
   * Hexahedron topology data.
   */
  static const int m_Edges[12][2];
  static const int m_Faces[6][4];
};


/**
 * Create the boundary-wrapped version of this cell type.
 */
template <typename TPixelType, typename TCellType>
class HexahedronBoundary:
  public CellBoundary< HexahedronCell< TPixelType , TCellType > >
{
public:
  typedef HexahedronBoundary  Self;
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  itkTypeMacro(HexahedronBoundary, CellBoundary);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHexahedronCell.txx"
#endif

#endif
