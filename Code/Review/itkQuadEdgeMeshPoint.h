/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshPoint_h
#define __itkQuadEdgeMeshPoint_h

#include "itkPoint.h"
#include "itkConceptChecking.h"
#include "itkGeometricalQuadEdge.h"

namespace itk
{
/**
 * \class QuadEdgeMeshPoint
 *
 * \brief Wrapper around a itk::Point in order to add a reference
 * to an entry in the edge ring.
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge =
            GeometricalQuadEdge< unsigned long, unsigned long, bool, bool, true > >
class QuadEdgeMeshPoint:public Point< TCoordRep, VPointDimension >
{
public:
  /** Standard typedefs. */
  typedef QuadEdgeMeshPoint                   Self;
  typedef Point< TCoordRep, VPointDimension > Superclass;

  /** Types & values defined in superclass. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      VPointDimension);

  typedef typename Superclass::ValueType     ValueType;
  typedef typename Superclass::CoordRepType  CoordRepType;
  typedef typename Superclass::RealType      RealType;
  typedef typename Superclass::BaseArray     BaseArray;
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::VectorType    VectorType;

  typedef ValueType ValueArrayType[itkGetStaticConstMacro(PointDimension)];

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  /** End concept checking */
#endif
public:
  QuadEdgeMeshPoint();
  virtual ~QuadEdgeMeshPoint() {}
  QuadEdgeMeshPoint(const Self & r);
  QuadEdgeMeshPoint(const Superclass & r);
  QuadEdgeMeshPoint(const ValueType r[VPointDimension]):Superclass(r)
  {
    this->Initialize();
  }

  Self & operator=(const Self & r);

  Self & operator=(const Superclass & r);

  Self & operator=(const ValueType r[VPointDimension]);

  /** Accessor on m_Edge */
  void SetEdge(TQuadEdge *inputEdge);

  /** Set the coordinates from a standard itk::Point */
  void SetPoint(const Superclass & point);

  /** Accessor on m_Edge */
  TQuadEdge * GetEdge();

  TQuadEdge * GetEdge() const;

  /** FIXME Documentation missing */
  bool IsInternal() const;

  /** FIXME Documentation missing */
  int GetValence() const;

protected:
  void Initialize();

protected:
  TQuadEdge *m_Edge;  /**< Entry edge for this point into an Onext ring */
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshPoint.txx"
#endif

#endif
