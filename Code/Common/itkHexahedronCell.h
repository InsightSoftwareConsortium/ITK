/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHexahedronCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * TCellTraits =
 *     Type information of mesh containing cell.
 *
 * \ingroup MeshObjects
 */

template < typename TCellInterface >
class HexahedronCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  typedef HexahedronCell      Self;
  typedef TCellInterface      Superclass;
//  typedef CellInterface<TPixelType,TCellTraits>  Superclass;
//  typedef SmartPointer<Self>  Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self *  Pointer;
  typedef const Self *  ConstPointer;
  
  /** Method for creation through the object factory. */
//  itkNewMacro(Self);
  static Pointer New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(HexahedronCell, CellInterface);

  /** Save the PixelType template parameter. */
  typedef typename Superclass::PixelType              PixelType;
  
  /** Save the CellTraits template parameter. */
  typedef typename Superclass::CellTraits             CellTraits;

  /** Pick-up typedefs from superclass */
  typedef typename Superclass::PointIdIterator        PointIdIterator;
  typedef typename Superclass::PointIdConstIterator   PointIdConstIterator;
  typedef typename Superclass::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier                       CellFeatureCount;
  
  /** Save some template parameter information. */
  typedef typename CellTraits::CoordRepType                     CoordRepType;
  typedef typename CellTraits::PointIdentifier                  PointIdentifier;
  typedef typename CellInterface<PixelType,CellTraits>::Pointer CellPointer;
  
  /** Save some template parameter information. */
  enum { PointDimension = CellTraits::PointDimension };

  /** The type of boundary for this hexahedron's vertices. */
  typedef VertexBoundary< TCellInterface >         Vertex;
  typedef typename Vertex::Pointer VertexPointer;
  
  /** The type of boundary for this hexahedron's edges. */
  typedef LineBoundary< TCellInterface >           Edge;
  typedef typename Edge::Pointer EdgePointer;
  
  /** The type of boundary for this hexahedron's faces. */
  typedef QuadrilateralBoundary< TCellInterface >  Face;
  typedef typename Face::Pointer FacePointer;
    
  /** Hexahedron-specific topology numbers. */
  enum { NumberOfPoints   =  8,
         NumberOfVertices =  8,
         NumberOfEdges    = 12,
         NumberOfFaces    =  6,
         CellDimension    =  3 };

  /** Implement the standard CellInterface. */
  virtual typename Superclass::CellType GetType(void) const 
    {return Superclass::HEXAHEDRON_CELL;}
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void) const;
  virtual int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first, PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 
  
  /** Hexahedron-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual CellFeatureCount GetNumberOfEdges(void) const;
  virtual CellFeatureCount GetNumberOfFaces(void) const;
  virtual VertexPointer  GetVertex(CellFeatureIdentifier);
  virtual EdgePointer    GetEdge(CellFeatureIdentifier);  
  virtual FacePointer    GetFace(CellFeatureIdentifier);  
  
  /** Visitor interface */
  itkCellVisitMacro(HEXAHEDRON_CELL);

protected:
  /** Store the number of points needed for a hexahedron. */
  PointIdentifier m_PointIds[NumberOfPoints];
  
  /** Hexahedron topology data. */
  static const int m_Edges[12][2];
  static const int m_Faces[6][4];
    
public:
  HexahedronCell() {}
  ~HexahedronCell() {}
  
private:
  HexahedronCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};


/** \class HexahedronBoundary
 * Create a boundary-wrapped version of the HexahedronCell.
 *
 * \ingroup MeshObjects
 *
 */
template <typename TCellInterface>
class HexahedronBoundary:
  public CellBoundary< HexahedronCell< TCellInterface > >
{
public:
  /** Standard class typedefs. */
  typedef HexahedronBoundary  Self;
//  typedef SmartPointer<Self>  Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self *  Pointer;
  typedef const Self *  ConstPointer;
    
  /** Method for creation through the object factory. */
//  itkNewMacro(Self);
  static Pointer New(void) { return new Self; }

  /** Standard part of every itk Object. */
  itkTypeMacro(HexahedronBoundary, CellBoundary);
protected:
  HexahedronBoundary() {}
  ~HexahedronBoundary() {}
  
private:
  HexahedronBoundary(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHexahedronCell.txx"
#endif

#endif
