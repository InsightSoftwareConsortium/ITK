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
#include "itkQuadrilateralCellTopology.h"

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
class ITK_EXPORT QuadrilateralCell: public TCellInterface, private QuadrilateralCellTopology
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(QuadrilateralCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexBoundary< TCellInterface >            VertexType;
  typedef typename VertexType::SelfAutoPointer        VertexAutoPointer;
  
  /** The type of boundary for this triangle's edges. */
  typedef LineBoundary< TCellInterface >              EdgeType;
  typedef typename EdgeType::SelfAutoPointer          EdgeAutoPointer;
 
  /** Quadrilateral-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 4);
  itkStaticConstMacro(NumberOfVertices, unsigned int, 4);
  itkStaticConstMacro(NumberOfEdges, unsigned int, 4);
  itkStaticConstMacro(CellDimension, unsigned int, 2);
  
  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const 
    {return Superclass::QUADRILATERAL_CELL;}
  virtual void MakeCopy( CellAutoPointer & ) const;
  virtual unsigned int GetDimension(void) const;
  virtual unsigned int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier,CellAutoPointer &);
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
  virtual bool GetVertex(CellFeatureIdentifier,VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier,EdgeAutoPointer &);
  
  /** Visitor interface */
  itkCellVisitMacro(QUADRILATERAL_CELL);

  /** Constructor and destructor */
  QuadrilateralCell() {};
  ~QuadrilateralCell() {};

protected:
  /** Store the number of points needed for a quadrilateral. */
  PointIdentifier m_PointIds[NumberOfPoints];

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
  itkCellCommonTypedefs(QuadrilateralBoundary);
 
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralBoundary, CellBoundary);

  /** Constructor and destructor */
  QuadrilateralBoundary() {}
  ~QuadrilateralBoundary() {}

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadrilateralCell.txx"
#endif

#endif
