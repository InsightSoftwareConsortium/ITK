/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVertexCell_h
#define __itkVertexCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"

namespace itk
{

/** \class VertexCell
 * VertexCell represents a single vertex for a Mesh.
 *
 * The CellBoundary wrapper for this cell is VertexBoundary.
 *
 * Template parameters for VertexCell:
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

template <
  typename TPixelType,
  typename TCellTraits
  >
class VertexCell: public CellInterface< TPixelType , TCellTraits >
{
public:
  /** Standard class typedefs. */
  typedef VertexCell          Self;
  typedef CellInterface<TPixelType,TCellTraits>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard part of every itk Object. */
  itkTypeMacro(VertexCell, CellInterface);

  /** Save the PixelType template parameter. */
  typedef TPixelType                                PixelType;
  
  /** Save the CellTraits template parameter. */
  typedef TCellTraits                                 CellTraits;

  /** Save some template parameter information. */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
  typedef typename CellInterface<TPixelType,TCellTraits>::Pointer CellPointer;
  
  /** Save some template parameter information. */
  enum { PointDimension = CellTraits::PointDimension };

  /** Vertex-specific topology numbers. */
  enum { NumberOfPoints = 1,
         CellDimension  = 0 };
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Pick-up typedefs from superclass or classes that we use. */
  typedef typename CellTraits::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdConstIterator
                   PointIdConstIterator;
  
  /** Implement the standard CellInterface. */
  virtual typename Superclass::CellType GetType(void) const 
    {return Superclass::VERTEX_CELL;}
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
    
  /** Vertex-specific interface. */
  virtual void SetPointId(PointIdentifier);
  virtual PointIdentifier GetPointId(void);
  
  /** Cell visitor interface */
  itkCellVisitMacro(VERTEX_CELL);

protected:
  VertexCell() {}
  ~VertexCell() {}

  /**
   * Store the number of points needed for a vertex.
   */
  PointIdentifier m_PointIds[NumberOfPoints];

 private:
  VertexCell(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

/** \class VertexBoundary
 * Create a boundary-wrapped version of the VertexCell.
 */
template <typename TPixelType, typename TCellTraits>
class VertexBoundary:
  public CellBoundary< VertexCell< TPixelType , TCellTraits > >
{
public:
  /** Standard class typdefs. */
  typedef VertexBoundary      Self;
  typedef SmartPointer<Self>  Pointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  
  
  /** Standard part of every itk Object. */
  itkTypeMacro(VertexBoundary, CellBoundary);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVertexCell.txx"
#endif

#endif
