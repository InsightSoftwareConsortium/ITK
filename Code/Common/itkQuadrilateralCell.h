/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadrilateralCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadrilateralCell_h
#define __itkQuadrilateralCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"

namespace itk
{

/** \class QuadrilateralCell
 * QuadrilateralCell represents a quadrilateral for a Mesh.
 *
 * The CellBoundary wrapper for this cell is QuadrilateralBoundary.
 *
 * Template parameters for QuadrilateralCell:
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
class QuadrilateralCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  typedef QuadrilateralCell   Self;
  typedef TCellInterface      Superclass;
//  typedef CellInterface<TPixelType,TCellTraits>  Superclass;
//  typedef SmartPointer<Self>  Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self *      Pointer;
  typedef const Self *      ConstPointer;
    
  /** Method for creation through the object factory. */
  //itkNewMacro(Self);
  static Pointer   New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralCell, CellInterface);

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
  enum { PointDimension = CellTraits::PointDimension };

  /**( Save some template parameter information. */
  typedef typename CellTraits::CoordRepType                     CoordRepType;
  typedef typename CellTraits::PointIdentifier                  PointIdentifier;
  typedef typename CellInterface<PixelType,CellTraits>::Pointer CellPointer;
  
  /** The type of boundary for this quadrilateral's vertices. */
  typedef VertexBoundary< TCellInterface >  Vertex;
  typedef typename Vertex::Pointer VertexPointer;
  
  /** The type of boundary for this quadrilateral's edges. */
  typedef LineBoundary< TCellInterface >              Edge;
  typedef typename Edge::Pointer                      EdgePointer;
    
  /** Quadrilateral-specific topology numbers. */
  enum { NumberOfPoints   = 4,
         NumberOfVertices = 4,
         NumberOfEdges    = 4,
         CellDimension    = 2 };
  
  /** Implement the standard CellInterface. */
  virtual typename Superclass::CellType GetType(void) const 
    {return Superclass::QUADRILATERAL_CELL;}
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
  
  /** Quadrilateral-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual CellFeatureCount GetNumberOfEdges(void) const;
  virtual VertexPointer GetVertex(CellFeatureIdentifier);
  virtual EdgePointer GetEdge(CellFeatureIdentifier);
  
  /** Visitor interface */
  itkCellVisitMacro(QUADRILATERAL_CELL);

public:
  QuadrilateralCell() {}
  ~QuadrilateralCell() {}

protected:
  /** Store the number of points needed for a quadrilateral. */
  PointIdentifier m_PointIds[NumberOfPoints];

  /** Quadrilateral topology data. */
  static const int m_Edges[4][2];
private:
  QuadrilateralCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


/** \class QuadrilateralBoundary
 * Create a boundary-wrapped version of the QuadrilateralCell. */
template <typename TCellInterface>
class QuadrilateralBoundary:
  public CellBoundary< QuadrilateralCell< TCellInterface > >
{
public:
  /** Standard class typedefs. */
  typedef QuadrilateralBoundary  Self;
//typedef SmartPointer<Self>     Pointer;
//typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self *      Pointer;
  typedef const Self *      ConstPointer;
    
  /** Method for creation through the object factory. */
  //itkNewMacro(Self);
  static Pointer   New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadrilateralCell.txx"
#endif

#endif
