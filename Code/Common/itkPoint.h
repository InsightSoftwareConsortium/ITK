/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkPoint_h
#define __itkPoint_h

#include "itkVector.h"
#include "vnl/vnl_vector_ref.h"
#include "itkIndent.h"


namespace itk
{

/** \class Point
 * \brief A templated class holding a geometric point in n-Dimensional space.
 * 
 * Point is a templated class that holds a set of coordinates (components).
 * Point can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The TPointDimension defines the number of
 * components in the point array. 
 *
 * 
 * \sa Image
 * \sa Mesh
 * \sa Vector
 * \sa CovariantVector
 * \sa Matrix
 *
 */

template<class TCoordRep, unsigned int TPointDimension=3>
class Point : public Array< TCoordRep, TPointDimension >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Point  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Array<TCoordRep,TPointDimension>  Superclass;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.  
   */
  typedef TCoordRep ValueType;
  typedef TCoordRep CoordRepType;

  /**
   * Dimension of the Space
   */
  enum { PointDimension = TPointDimension };

  /**
   * The Array type from which this Vector is derived.
   */
  typedef Array<TCoordRep, TPointDimension>         BaseArray;
  typedef typename BaseArray::ArrayCommaListCopier  ArrayCommaListCopier;
  typedef typename BaseArray::Iterator              Iterator;
  typedef typename BaseArray::ConstIterator         ConstIterator;
  
  /**
   * Get the dimension (size) of the point.
   */
  static unsigned int GetPointDimension() 
    { return TPointDimension; }
  
  /**
   * VectorType define the difference between two Points
   */
  typedef Vector< ValueType, TPointDimension >   VectorType;

  /**
   * Default constructor has nothing to do.
   */
  Point() {}

  /*@{
   * Pass-through constructor for the Array base class.
   */
  Point(const Self& r): BaseArray(r) {}
  Point(const typename BaseArray::Reference& r): BaseArray(r) {}
  Point(const typename BaseArray::ConstReference& r): BaseArray(r) {}
  Point(const ValueType r[PointDimension]): BaseArray(r) {}  
  //@}
  
  /*@{
   * Pass-through assignment operator for the Array base class.
   */
  Point& operator= (const Self& r);
  Point& operator= (const typename BaseArray::Reference& r);
  Point& operator= (const typename BaseArray::ConstReference& r);
  Point& operator= (const ValueType r[PointDimension]);
  ArrayCommaListCopier operator= (const ValueType& r);
  //@}
  
  /**
   * Point operator+=.  Adds a vector to the current point.
   */
  const Self& operator+=(const VectorType &vec);


  /**
   * Point operator-=.  Subtracts a vector from a current point.
   */
  const Self& operator-=(const VectorType &vec);


  /**
   * Computes the Vector difference between two points
   */
  VectorType operator-(const Self &pnt) const;



  /**
   * Add a vector to a point. Return a new point.
   */
  Self operator+(const VectorType &vec) const;
  

   /**
   * Subtract a vector from a point. Return a new point.
   */
  Self operator-(const VectorType &vec) const;
  

  /**
   * Access an element of a point. This version can be used as an lvalue.
   */
  VectorType GetVectorFromOrigin( void ) const;


  /**
   * Returns Euclidean distance between two points
   */
  TCoordRep EuclideanDistanceTo( const Point &  ) const;

  
  /**
   * Returns Squared Euclidean distance between two points
   */
  TCoordRep SquaredEuclideanDistanceTo( const Point &  ) const;
  

  /**
   * Get a vnl_vector_ref referencing the same memory block
   */
  vnl_vector_ref<TCoordRep> Get_vnl_vector( void );
};

  

template< class T, unsigned int TPointDimension >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const Point<T,TPointDimension> & v); 

template< class T, unsigned int TPointDimension >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                    Point<T,TPointDimension> & v); 

} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.txx"
#endif


#endif 
