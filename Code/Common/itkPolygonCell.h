/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPolygonCell_h
#define __itkPolygonCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"
#include "itkPoint.h"
#include <vector>
#include <queue>


namespace itk
{

/** \class PolygonCell
 * PolygonCell represents a polygon for a Mesh.
 *  the points of the polygon can be dynamically changed.
 *
 * Template parameters for PolygonCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 *
 * TCellTraits =
 *     Type information of mesh containing cell.
 * \ingroup MeshObjects
 */
template < typename TCellInterface >
class PolygonCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  typedef PolygonCell  Self;
  typedef TCellInterface  Superclass;
//  typedef CellInterface<TPixelType,TCellTraits>  Superclass;
//  typedef SmartPointer<Self>  Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self * Pointer;
  typedef const Self * ConstPointer;
    
  /** Save the PixelType template parameter. */
  typedef typename Superclass::PixelType              PixelType;
  
  /** Save the CellTraits template parameter. */
  typedef typename Superclass::CellTraits             CellTraits;

  /** Extract typedefs from superclass. */
  typedef typename Superclass::PointIdIterator        PointIdIterator;
  typedef typename Superclass::PointIdConstIterator   PointIdConstIterator;
  typedef typename Superclass::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier                       CellFeatureCount;
  
  /** Save some template parameter information. */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
    
  /** Save some template parameter information. */
  enum { PointDimension = CellTraits::PointDimension};
  enum { CellDimension = 2 };
    
  /** Define a typedef to the cell interface. */
  typedef typename CellInterface<PixelType,CellTraits>::Pointer CellPointer;

  /** The type of boundary for this voronoi cell's vertices. */
  typedef VertexBoundary< TCellInterface >  Vertex;
  typedef typename Vertex::Pointer          VertexPointer;
  
  /** The type of boundary for this voronoi cell's edges. */
  typedef LineBoundary< TCellInterface >    Edge;
  typedef typename Edge::Pointer EdgePointer;
  typedef FixedArray<int,2> EdgeInfo;
  typedef std::deque<EdgeInfo> EdgeInfoDQ;
  
  /** Method for creation through the object factory. */
  // itkNewMacro(Self);
  static Pointer New(void) { return new Self; }

  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonCell, CellInterface);

  /** Need to add POLYGON_CELL into CellInterface. */
  itkCellVisitMacro(POLYGON_CELL);

  /** Implement the standard CellInterface. */
  virtual typename Superclass::CellType GetType(void) const 
    {return Superclass::POLYGON_CELL;}
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void) const;
  virtual int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  
  virtual void SetPointIds(PointIdConstIterator first); 
  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);

  void AddPointId(PointIdentifier);
  void SetPointIds(int dummy, int num, PointIdConstIterator first);
  void BuildEdges(void);
  void ClearPoints(void);
  
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 
  
  /** Polygon-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual CellFeatureCount GetNumberOfEdges(void) const;
  virtual VertexPointer GetVertex(CellFeatureIdentifier);
  virtual EdgePointer GetEdge(CellFeatureIdentifier);
  
protected:
  std::vector<EdgeInfo> m_Edges;
  std::vector<PointIdentifier> m_PointIds;

public:
  PolygonCell() {}
  ~PolygonCell() {}

private:
  PolygonCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

/** \class PolygonBoundary
 * Create a boundary-wrapped version of the PolygonCell.
 *
 * \ingroup MeshObjects
 */
template <typename TCellInterface >
class PolygonBoundary:
  public CellBoundary< PolygonCell< TCellInterface > >
{
public:
  /** Standard class typedefs. */
  typedef PolygonBoundary  Self;
  //typedef SmartPointer<Self>     Pointer;
//  typedef SmartPointer<const Self>  ConstPointer;
  typedef       Self * Pointer;
  typedef const Self * ConstPointer;
    
  /** Method for creation through the object factory. */
  //itkNewMacro(Self);
  static Pointer New(void) { return new Self; }
  
  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonBoundary, CellBoundary);
};

} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonCell.txx"
#endif

#endif


