// -------------------------------------------------------------------------
// itkQEPoint.h
// $Revision: 1.3 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-16 17:44:37 $
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

#ifndef __ITKQUADEDGEMESH__POINT__H__
#define __ITKQUADEDGEMESH__POINT__H__

#include <itkPoint.h>

namespace itkQE
{
/**
 * Wrapper around a itk::Point in order to add a reference
 * to an entry in the edge ring.
 */
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
class Point
    : public itk::Point< TCoordRep, VPointDimension >
{
  public:
    /** Standard typedefs. */
    typedef Point                                    Self;
    typedef itk::Point< TCoordRep, VPointDimension > Superclass;

    /** Types & values defined in superclass. */
    itkStaticConstMacro( PointDimension, unsigned int,
                         Superclass::PointDimension );
    typedef typename Superclass::ValueType     ValueType;
    typedef typename Superclass::CoordRepType  CoordRepType;
    typedef typename Superclass::RealType      RealType;
    typedef typename Superclass::BaseArray     BaseArray;
    typedef typename Superclass::Iterator      Iterator;
    typedef typename Superclass::ConstIterator ConstIterator;
    typedef typename Superclass::VectorType    VectorType;

    typedef ValueType ValueArrayType[ 
      itkGetStaticConstMacro( PointDimension ) ];

  public:
    Point( );
    Point( const Self& r );
    Point( const ValueArrayType & r );
    Point( const VectorType& vec );
    Self& operator=( const Self& r );
    Self& operator=( const Superclass& r );
    Self& operator=( const ValueArrayType & r );
    Self& operator=( const VectorType& vec );

    /** Accessor on \ref m_Edge */
    void SetEdge( QEType* in_Edge );
    /** Accessor on \ref m_Edge */
    QEType* GetEdge() const;

    bool IsInternal( );
    int GetValence( );

  protected:
    void Initialise( );

  protected:
    QEType* m_Edge; /// Entry edge for this point into an Onext ring
};

} // fnamespace

#include "itkQEPoint.txx"

#endif // __ITKQUADEDGEMESH__POINT__H__

// eof - itkQEPoint.h
