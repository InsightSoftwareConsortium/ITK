/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
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
 * \ingroup Geometry
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
   * Compare two points for equality.
   */
  bool
  operator==(const Self &pt) const
    {
    bool same=true;
    for (unsigned int i=0; i < PointDimension && same; i++)
      { same = ((*this)[i] == pt[i]); }
    return same;
    }

  /**
   * Compare two points for inequality.
   */
  bool
  operator!=(const Self &pt) const
    {
    bool same=true;
    for (unsigned int i=0; i < PointDimension && same; i++)
      { same = ((*this)[i] == pt[i]); }
    return !same;
    }

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


  /**
   * Set to median point between the two points
   * given as arguments
   *
   * This method computes:
   *
   * \f[
   *   \overrightarrow{P}=\frac{(\overrightarrow{A}+\overrightarrow{B})}{2}
   * \f]
   *
   * using the two Points given as arguments, and store the result in 
   * the Point on which the method is invoked.
   */
  void SetToMidPoint( const Point &, const Point &  );


  /**
   * Set the current point to a barycentric combination of the two points
   * given as arguments.
   *
   * \parameter \f$ \alpha \f$ = weight for the first point
   * 
   * The first point is multiplied by \f$ \alpha \f$, the second is multiplied 
   * by * \f$ (1-\alpha) \f$, and the sum is stored in the Point on which the
   * method is invoked.
   *
   * \f[
   *   \overrightarrow{P}=\alpha * \overrightarrow{A}+ (1-\alpha)*\overrightarrow{B}
   * \f]
   *
   * If the value of \f$ \alpha \in [0,1] \f$, the resulting point will be placed
   * in the line segment \f$ \overline{AB} \f$ joining  \f$ \overrightarrow{A} \f$ 
   * and \f$  \overrightarrow{A} \f$
   * 
   * If the value of \f$ \alpha < 0 \f$ the resulting point will be placed outside 
   * the line segment   \f$ \overline{AB} \f$ on the side of \f$ \overrightarrow{A} \f$.
   *
   * If the value of \f$ \alpha > 1 \f$ the resulting point will be placed outside 
   * the line segment   \f$ \overline{AB} \f$ on the side of \f$ \overrightarrow{B} \f$.
   *
   * \sa SetToMedian
   */
  void SetToBarycentricCombination( const Point & A, const Point & B, double alpha   );
 

  /**
   * Set the current point to a barycentric combination of three points
   * Two values are expected to weight the contribution of the first two points,
   * the weight of for the third point is computed to ensure that the three weights
   * sum 1.
   *
   * This method computes:
   *
   * \f[
   *   \overrightarrow{P}=     w_1        * \overrightarrow{P}_1 
                          +    w_2        * \overrightarrow{P}_2 
                          +  (1-w_1-w_2 ) * \overrightarrow{P}_3 
   * \f]
   *
   * If the two weight are \f$ \in [0,1], The resulting point will alway be placed 
   * inside the triangle formed by the three points given as arguments.
   * 
   * 
   */
  void SetToBarycentricCombination( const Point & A, const Point & B, const Point & C, 
                                    double weightA,  double weightB );
 



  /**
   * Set the current point to a barycentric combination of an array of N points
   * An array of (N-1) values is expected to weight the contribution of the 
   * first (N-1) points, the weight of the Nth point is computed to ensure that 
   * the N weights sum 1.
   *
   * This method computes:
   *
   * \f[
   *   \overrightarrow{P}=    \sum_{i=1}^{N-1} w_i * \overrightarrow{P}_i 
          +   \left(1- \sum_{i=1}^{N-1} w_i\right) * \overrightarrow{P}_N 
   * \f]
   *
   * 
   */
  void SetToBarycentricCombination( const Point * P, const double * weights, unsigned int N);
 

};

  

template< class T, unsigned int TPointDimension >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const Point<T,TPointDimension> & v); 

template< class T, unsigned int TPointDimension >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                    Point<T,TPointDimension> & v); 


/**
 * Class that computes the barycentric combination of an array of N points
 *
 * An array of (N-1) values is expected to weight the contribution of the 
 * first (N-1) points, the weight of the Nth point is computed to ensure that 
 * the N weights sum 1.
 *
 * This method computes:
 *
 * \f[
 *   \overrightarrow{P}=    \sum_{i=1}^{N-1} w_i * \overrightarrow{P}_i 
 *      +   \left(1- \sum_{i=1}^{N-1} w_i\right) * \overrightarrow{P}_N 
 * \f]
 *
 * The points are expected to be stored in an itkContainer class like 
 * itk::VectorContainer, responding to the Begin(), End(), Value() API.
 *
 * The weights are expected to be stored in any array-like container
 * having a operator[i].
 * 
 */
template< class TPointContainer, class TWeightContainer >
ITK_EXPORT class BarycentricCombination  
{

public:
  BarycentricCombination() {}; 
  ~BarycentricCombination() {};
  static typename TPointContainer::Element Evaluate( 
                      const typename TPointContainer::Pointer & points, 
                      const TWeightContainer & weights );
};
  
};  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.txx"
#endif


#endif 
