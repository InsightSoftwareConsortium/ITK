/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadraticTriangleCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadraticTriangleCell_h
#define __itkQuadraticTriangleCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkQuadraticEdgeCell.h"

namespace itk
{

/** \class QuadraticTriangleCell
 * QuadraticTriangleCell represents a second order triangular patch for a Mesh.
 *
 * The CellBoundary wrapper for this cell is QuadraticTriangleBoundary.
 *
 * Template parameters for QuadraticTriangleCell:
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
class QuadraticTriangleCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(QuadraticTriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadraticTriangleCell, CellInterface);
  
  /** The type of boundary for this triangle's vertices. */
  typedef VertexBoundary< TCellInterface >            VertexType;
  typedef typename VertexType::SelfAutoPointer        VertexAutoPointer;
  
  /** The type of boundary for this triangle's edges. */
  typedef QuadraticEdgeBoundary< TCellInterface >     EdgeType;
  typedef typename EdgeType::SelfAutoPointer          EdgeAutoPointer;
    
  /** Triangle-specific topology numbers. */
  enum { NumberOfPoints   = 6,
         NumberOfVertices = 3,
         NumberOfEdges    = 3,
         CellDimension    = 2 };
  
  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const 
    {return Superclass::QUADRATIC_TRIANGLE_CELL;}
  virtual void MakeCopy( CellAutoPointer & ) const;
  virtual unsigned int GetDimension(void) const;
  virtual unsigned int GetNumberOfPoints(void) const;
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;
  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &);
  virtual void SetPointIds(PointIdConstIterator first);
  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 
  
  /** Triangle-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual CellFeatureCount GetNumberOfEdges(void) const;
  virtual bool  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);
  
  /** Cell visitor interface. */
  itkCellVisitMacro(QUADRATIC_TRIANGLE_CELL);


  /** Given the parametric coordinates of a point in the cell
   *  determine the value of its Shape Functions
   *  returned through an itkArray<InterpolationWeightType>).  */
  virtual void EvaluateShapeFunctions( 
                          const ParametricCoordArrayType & parametricCoordinates,
                                ShapeFunctionsArrayType  & weights) const;



 public:
  QuadraticTriangleCell() {}
  ~QuadraticTriangleCell() {}

 protected:
  /** Store the number of points needed for a triangle. */
  PointIdentifier m_PointIds[NumberOfPoints];

  /** Triangle topology data. */
  static const int m_Edges[3][3]; // 3 quadratic edges with 3 points each

 private:
  QuadraticTriangleCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};


/** \class TriangleBoundary
 * Create a boundary-wrapped version of the QuadraticTriangleCell.
 */
template <typename TCellInterface>
class QuadraticTriangleBoundary:
  public CellBoundary< QuadraticTriangleCell< TCellInterface > >
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(QuadraticTriangleBoundary);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadraticTriangleBoundary, CellBoundary);

  /** Constructor and destructor */
  QuadraticTriangleBoundary() {};
  ~QuadraticTriangleBoundary() {};

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadraticTriangleCell.txx"
#endif

#endif
