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

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkQuadrilateralCell.h"

namespace itk
{

/** \class HexahedronCell
 * HexahedronCell represents a hexahedron for a Mesh.
 *
 * The CellBoundary wrapper for this cell is HexahedronBoundary.
 *
 * Template parameters for HexahedronCell:
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
class HexahedronCell: public CellInterface< TPixelType , TCellType >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef HexahedronCell      Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef CellInterface<TPixelType,TCellType>  Superclass;

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

  /**
   * Pick-up typedefs from superclass
   */
  typedef typename CellType::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellType>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellType>::PointIdConstIterator
                   PointIdConstIterator;

  /**
   * Save some template parameter information.
   */
  typedef typename CellType::CoordRep         CoordRep;
  typedef typename CellType::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellType::PointDimension };
  typedef typename CellInterface<TPixelType,TCellType>::Pointer CellPointer;

  /**
   * The type of boundary for this hexahedron's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellType >         Vertex;
  typedef typename Vertex::Pointer VertexPointer;

  /**
   * The type of boundary for this hexahedron's edges.
   */
  typedef LineBoundary< TPixelType , TCellType >           Edge;
  typedef typename Edge::Pointer EdgePointer;

  /**
   * The type of boundary for this hexahedron's faces.
   */
  typedef QuadrilateralBoundary< TPixelType , TCellType >  Face;
  typedef typename Face::Pointer FacePointer;
  
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
   * Implement the standard CellInterface.
   */
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void);
  virtual int GetNumberOfPoints(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first, PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 

  /**
   * Hexahedron-specific interface.
   */
  
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual CellFeatureCount GetNumberOfFaces(void);
  virtual VertexPointer  GetVertex(CellFeatureIdentifier);
  virtual EdgePointer    GetEdge(CellFeatureIdentifier);  
  virtual FacePointer    GetFace(CellFeatureIdentifier);  

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(HexahedronCell, CellInterface);
  /**
   * Visitor interface
   */
  itkCellVisitMacro(HEXAHEDRON_CELL);
protected:
  /**
   * Store the number of points needed for a hexahedron.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
  
  /**
   * Hexahedron topology data.
   */
  static const int m_Edges[12][2];
  static const int m_Faces[6][4];
};


/** \class HexahedronBoundary
 * Create a boundary-wrapped version of the HexahedronCell.
 */
template <typename TPixelType, typename TCellType>
class HexahedronBoundary:
  public CellBoundary< HexahedronCell< TPixelType , TCellType > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef HexahedronBoundary  Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(HexahedronBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHexahedronCell.txx"
#endif

#endif
