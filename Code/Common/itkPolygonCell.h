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
class ITK_EXPORT PolygonCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(PolygonCell);
  itkCellInheritedTypedefs(TCellInterface);
   
  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonCell, CellInterface);

  /** Save some template parameter information. */
  itkStaticConstMacro(CellDimension, unsigned int, 2);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexBoundary< TCellInterface >            VertexType;
  typedef typename VertexType::SelfAutoPointer        VertexAutoPointer;
  
  /** The type of boundary for this triangle's edges. */
  typedef LineBoundary< TCellInterface >              EdgeType;
  typedef typename EdgeType::SelfAutoPointer          EdgeAutoPointer;
    
  typedef FixedArray<int,2> EdgeInfo;
  typedef std::deque<EdgeInfo> EdgeInfoDQ;
  
 /** Need to add POLYGON_CELL into CellInterface. */
  itkCellVisitMacro(POLYGON_CELL);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const 
    {return Superclass::POLYGON_CELL;}
  virtual void MakeCopy( CellAutoPointer & ) const;
  virtual unsigned int GetDimension(void) const;
  virtual unsigned int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier,CellAutoPointer &);
  
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
  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);
  

  /** Constructor and destructor */
  PolygonCell() {}
  ~PolygonCell() {}

protected:
  std::vector<EdgeInfo> m_Edges;
  std::vector<PointIdentifier> m_PointIds;

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
  itkCellCommonTypedefs(PolygonBoundary);

  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonBoundary, CellBoundary);

  /** Constructor and destructor */
  PolygonBoundary() {}
  ~PolygonBoundary() {}

};

} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonCell.txx"
#endif

#endif


