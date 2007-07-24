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
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __itkQuadEdgeMeshPoint_h
#define __itkQuadEdgeMeshPoint_h

#include "itkPoint.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class QuadEdgeMeshPoint
 * 
 * \brief Wrapper around a itk::Point in order to add a reference
 * to an entry in the edge ring.
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
class QuadEdgeMeshPoint : public Point< TCoordRep, VPointDimension >
{
public:
  /** Standard typedefs. */
  typedef QuadEdgeMeshPoint                         Self;
  typedef Point< TCoordRep, VPointDimension >       Superclass;
  
  /** Types & values defined in superclass. */
  itkStaticConstMacro( PointDimension, unsigned int,
                       VPointDimension );
  typedef typename Superclass::ValueType     ValueType;
  typedef typename Superclass::CoordRepType  CoordRepType;
  typedef typename Superclass::RealType      RealType;
  typedef typename Superclass::BaseArray     BaseArray;
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::VectorType    VectorType;

  typedef ValueType ValueArrayType[ itkGetStaticConstMacro( PointDimension ) ];

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(DimensionShouldBe3,
                  (Concept::SameDimension<itkGetStaticConstMacro(PointDimension),3>));
  /** End concept checking */
#endif

public:
  QuadEdgeMeshPoint();
  QuadEdgeMeshPoint( const Self & r );
  QuadEdgeMeshPoint( const ValueType r[VPointDimension] ):Superclass( r )
    {
    this->Initialize();
    } 
  Self & operator=( const Self & r );
  Self & operator=( const Superclass & r );
  Self & operator=( const ValueType r[VPointDimension] );
  
 
  /** Accessor on \ref m_Edge */
  void SetEdge( const TQuadEdge * inputEdge );

  /** Accessor on \ref m_Edge */
  const TQuadEdge * GetEdge();
  const TQuadEdge * GetEdge() const;

  /** FIXME Documentation missing */
  bool IsInternal() const;

  /** FIXME Documentation missing */
  int GetValence() const;

protected:
  void Initialize();

protected:
  const TQuadEdge * m_Edge; /// Entry edge for this point into an Onext ring
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshPoint.txx"
#endif

#endif 
