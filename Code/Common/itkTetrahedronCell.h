/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTetrahedronCell_h
#define __itkTetrahedronCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkTriangleCell.h"

namespace itk
{

/** \class TetrahedronCell
 * TetrahedronCell represents a tetrahedron for a Mesh.
 *
 * The CellBoundary wrapper for this cell is TetrahedronBoundary.
 *
 * Template parameters for TetrahedronCell:
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
class TetrahedronCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TetrahedronCell     Self;
  
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

  /**
   * The type of boundary for this tetrahedron's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellType >    Vertex;

  /**
   * The type of boundary for this tetrahedron's edges.
   */
  typedef LineBoundary< TPixelType , TCellType >      Edge;

  /**
   * The type of boundary for this tetrahedron's faces.
   */
  typedef TriangleBoundary< TPixelType , TCellType >  Face;
  
  /** \enum
   * Tetrahedron-specific topology numbers.
   */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 6,
         NumberOfFaces    = 4,
         CellDimension    = 3 };

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Implement the standard CellInterface.
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
   * Tetrahedron-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual CellFeatureCount GetNumberOfFaces(void);
  virtual Vertex::Pointer GetCellVertex(CellFeatureIdentifier);
  virtual Edge::Pointer GetCellEdge(CellFeatureIdentifier);  
  virtual Face::Pointer GetCellFace(CellFeatureIdentifier);  

  /**
   * Standard part of Object class.  Used for debugging output.
   */
  itkTypeMacro(TetrahedronCell, CellInterface);

protected:
  /**
   * Store the number of points needed for a tetrahedron.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
  
  /**
   * Tetrahedron topology data.
   */
  static const int m_Edges[6][2];
  static const int m_Faces[4][3];
};


/** \class TetrahedronBoundary
 * Create a boundary-wrapped version of the TetrahedronCell.
 */
template <typename TPixelType, typename TCellType>
class TetrahedronBoundary:
  public CellBoundary< TetrahedronCell< TPixelType , TCellType > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TetrahedronBoundary  Self;
  
  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>   Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Standard part of Object class.
   */
  itkTypeMacro(TetrahedronBoundary, CellBoundary);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTetrahedronCell.txx"
#endif

#endif
