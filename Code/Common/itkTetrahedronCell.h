/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \ingroup MeshObjects
 */
template <
  typename TPixelType,
  typename TCellTraits
  >
class TetrahedronCell: public CellInterface< TPixelType , TCellTraits >
{
public:
  /** Standard class typedefa. */
  typedef TetrahedronCell     Self;
  typedef CellInterface<TPixelType,TCellTraits>  Superclass;
//  typedef SmartPointer<Self>  Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self * Pointer;
  typedef const Self * ConstPointer;
  
  /** Method for creation through the object factory. */
  //itkNewMacro(Self);
  static Pointer New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(TetrahedronCell, CellInterface);

  /** Save the PixelType template parameter. */
  typedef TPixelType                                PixelType;
  
  /** Save the CellTraits template parameter. */
  typedef TCellTraits                                 CellTraits;

  /** Pick-up typedefs from superclass */
  typedef typename CellTraits::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdConstIterator
                   PointIdConstIterator;
  
  /** Save some template parameter from cell traits. */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
  typedef typename CellInterface<TPixelType,TCellTraits>::Pointer CellPointer;
  
  /** Save some template parameter from cell traits. */
  enum { PointDimension = CellTraits::PointDimension };

  /** The type of boundary for this tetrahedron's vertices. */
  typedef VertexBoundary< TPixelType , TCellTraits >    Vertex;
  typedef typename Vertex::Pointer VertexPointer;
  
  /** The type of boundary for this tetrahedron's edges. */
  typedef LineBoundary< TPixelType , TCellTraits >      Edge;
  typedef typename Edge::Pointer EdgePointer;
  
  /** The type of boundary for this tetrahedron's faces. */
  typedef TriangleBoundary< TPixelType , TCellTraits >  Face;
  typedef typename Face::Pointer FacePointer;
    
  /** Tetrahedron-specific topology numbers. */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 6,
         NumberOfFaces    = 4,
         CellDimension    = 3 };

  /** Implement the standard CellInterface. */
  virtual typename Superclass::CellType GetType(void) const 
    {return Superclass::TETRAHEDRON_CELL;}
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void) const;
  virtual int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 
  
  /** Tetrahedron-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual CellFeatureCount GetNumberOfEdges(void) const;
  virtual CellFeatureCount GetNumberOfFaces(void) const;
  virtual VertexPointer  GetVertex(CellFeatureIdentifier);
  virtual EdgePointer    GetEdge(CellFeatureIdentifier);  
  virtual FacePointer    GetFace(CellFeatureIdentifier);  
  
  /** Visitor interface. */
  itkCellVisitMacro(TETRAHEDRON_CELL);
  
 public:
  TetrahedronCell() {}
  ~TetrahedronCell() {}

 protected:
  /** Store the number of points needed for a tetrahedron. */
  PointIdentifier m_PointIds[NumberOfPoints];
  
  /** Tetrahedron topology data. */
  static const int m_Edges[6][2];
  static const int m_Faces[4][3];

 private:
  TetrahedronCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  };

/** \class TetrahedronBoundary
 * Create a boundary-wrapped version of the TetrahedronCell.
 */
template <typename TPixelType, typename TCellTraits>
class TetrahedronBoundary:
  public CellBoundary< TetrahedronCell< TPixelType , TCellTraits > >
{
public:
  /** Standard class typedefs. */
  typedef TetrahedronBoundary  Self;
//  typedef SmartPointer<Self>   Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self * Pointer;
  typedef const Self * ConstPointer;
    
  /** Method for creation through the object factory. */
  //itkNewMacro(Self);
  static Pointer New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(TetrahedronBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTetrahedronCell.txx"
#endif

#endif
