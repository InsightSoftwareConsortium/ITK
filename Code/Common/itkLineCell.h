/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLineCell_h
#define __itkLineCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkVertexCell.h"

namespace itk
{

/** \class LineCell
 * LineCell represents a line segment for a Mesh.
 *
 * The CellBoundary wrapper for this cell is LineBoundary.
 *
 * Template parameters for LineCell:
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
class ITK_EXPORT LineCell: public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(LineCell);
  itkCellInheritedTypedefs(TCellInterface);
  
  /** Standard part of every itk Object. */
  itkTypeMacro(LineCell, CellInterface);

  /** The type of boundary for this lines's vertices. */
  typedef VertexBoundary< TCellInterface >     VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;
    
  /** Line-specific topology numbers. */
  enum { NumberOfPoints   = 2,
         NumberOfVertices = 2,
         CellDimension    = 1 };
  
  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const 
  {return Superclass::LINE_CELL;}
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
  
  /** Line-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;
  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  
  /** Visitor interface */
  itkCellVisitMacro(LINE_CELL);

  LineCell() {}
  ~LineCell() {}

protected:
  /** Store number of points needed for a line segment. */
  PointIdentifier m_PointIds[NumberOfPoints];

 private:
  LineCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};


/** \class LineBoundary
 * Create a boundary-wrapped version of the LineCell.
 *
 * \ingroup MeshObjects
 */
template <typename TCellInterface>
class LineBoundary:
  public CellBoundary< LineCell< TCellInterface > >
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(LineBoundary);

  /** Standard part of every itk Object. */
  itkTypeMacro(LineBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.txx"
#endif

#endif
