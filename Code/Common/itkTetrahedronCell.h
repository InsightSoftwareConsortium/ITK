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
 * TCellTraits =
 *     Type information of mesh containing cell.
 */

template <
  typename TPixelType,
  typename TCellTraits
  >
class TetrahedronCell: public CellInterface< TPixelType , TCellTraits >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TetrahedronCell     Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef CellInterface<TPixelType,TCellTraits>  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Save the PixelType template parameter.
   */
  typedef TPixelType                                PixelType;
  
  /**
   * Save the CellTraits template parameter.
   */
  typedef TCellTraits                                 CellTraits;

  /**
   * Pick-up typedefs from superclass
   */
  typedef typename CellTraits::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdConstIterator
                   PointIdConstIterator;

  /**
   * Save some template parameter information.
   */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
  enum { PointDimension = CellTraits::PointDimension };
  typedef typename CellInterface<TPixelType,TCellTraits>::Pointer CellPointer;

  /**
   * The type of boundary for this tetrahedron's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellTraits >    Vertex;
  typedef typename Vertex::Pointer VertexPointer;

  /**
   * The type of boundary for this tetrahedron's edges.
   */
  typedef LineBoundary< TPixelType , TCellTraits >      Edge;
  typedef typename Edge::Pointer EdgePointer;

  /**
   * The type of boundary for this tetrahedron's faces.
   */
  typedef TriangleBoundary< TPixelType , TCellTraits >  Face;
  typedef typename Face::Pointer FacePointer;
  
  /**
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
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void);
  virtual int GetNumberOfPoints(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first,
			   PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 

  /**
   * Tetrahedron-specific interface.
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
  itkTypeMacro(TetrahedronCell, CellInterface);
  /**
   * Visitor interface
   */
  itkCellVisitMacro(TETRAHEDRON_CELL);
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
template <typename TPixelType, typename TCellTraits>
class TetrahedronBoundary:
  public CellBoundary< TetrahedronCell< TPixelType , TCellTraits > >
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
   * Standard part of every itk Object.
   */
  itkTypeMacro(TetrahedronBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTetrahedronCell.txx"
#endif

#endif
