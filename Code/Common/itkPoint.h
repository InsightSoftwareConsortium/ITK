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

template<class T, unsigned int TPointDimension=3>
class Point : public Array< T, TPointDimension > {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef Point  Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.  
   */
  typedef T ValueType;


  /**
   * Dimension of the Space
   */
  enum { PointDimension = TPointDimension };


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
   * Operator=.  Assign a point to a point.
   */
  const Self& operator=(const Self& point);


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
  VectorType operator-(const Self &point) const;



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
  
  
};

  
} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.txx"
#endif


#endif 
