/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPoint_h
#define itkPoint_h


#include "itkNumericTraits.h"
#include "itkVector.h"

#include "vnl/vnl_vector_ref.h"
#include "itkMath.h"

namespace itk
{
/** \class Point
 * \brief A templated class holding a geometric point in n-Dimensional space.
 *
 * Point is a templated class that holds a set of coordinates (components).
 * Point can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The NPointDimension defines the number of
 * components in the point array.
 *
 * \ingroup Geometry
 * \ingroup DataRepresentation
 *
 * \sa Image \sa Mesh \sa Vector \sa CovariantVector \sa Matrix
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/DistanceBetweenPoints,Distance between two points}
 * \wikiexample{SimpleOperations/DistanceBetweenIndices,Distance between two indices}
 * \endwiki
 */
template< typename TCoordRep, unsigned int NPointDimension = 3 >
class ITK_TEMPLATE_EXPORT Point:public FixedArray< TCoordRep, NPointDimension >
{
public:
  /** Standard class typedefs. */
  typedef Point                                    Self;
  typedef FixedArray< TCoordRep, NPointDimension > Superclass;

  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.   */
  typedef TCoordRep ValueType;
  typedef TCoordRep CoordRepType;

  typedef typename NumericTraits< ValueType >::RealType RealType;

  /** Dimension of the Space */
  itkStaticConstMacro(PointDimension, unsigned int, NPointDimension);

  /** The Array type from which this Vector is derived. */
  typedef FixedArray< TCoordRep, NPointDimension > BaseArray;
  typedef typename BaseArray::Iterator             Iterator;
  typedef typename BaseArray::ConstIterator        ConstIterator;

  /** Get the dimension (size) of the point. */
  static unsigned int GetPointDimension()
  { return NPointDimension; }

  /** VectorType define the difference between two Points */
  typedef Vector< ValueType, NPointDimension > VectorType;

  /** Default constructor has nothing to do. */
  Point() {}

  /** Pass-through constructors for the Array base class. */
  template< typename TPointValueType >
  Point(const Point< TPointValueType, NPointDimension > & r):BaseArray(r) {}
  template< typename TPointValueType >
  Point(const TPointValueType r[NPointDimension]):BaseArray(r) {}
  Point(const ValueType r[NPointDimension]):BaseArray(r) {}
  template< typename TPointValueType >
  Point(const TPointValueType & v):BaseArray(v) {}
  Point(const ValueType & v):BaseArray(v) {}

  /** Pass-through assignment operator for the Array base class. */
  Point & operator=(const Self & r);

  Point & operator=(const ValueType r[NPointDimension]);

  /** Compare two points for equality. */
  bool
  operator==(const Self & pt) const
  {
    bool same = true;

    for ( unsigned int i = 0; i < NPointDimension && same; ++i )
          { same = ( Math::ExactlyEquals(( *this )[i], pt[i]) ); }
    return same;
  }

  /** Compare two points for inequality. */
  bool
  operator!=(const Self & pt) const
  {
    bool same = true;

    for ( unsigned int i = 0; i < NPointDimension && same; ++i )
          { same = ( Math::ExactlyEquals(( *this )[i], pt[i]) ); }
    return !same;
  }

  /** Point operator+=.  Adds a vector to the current point. */
  const Self & operator+=(const VectorType & vec);

  /** Point operator-=.  Subtracts a vector from a current point. */
  const Self & operator-=(const VectorType & vec);

  /** Computes the Vector difference between two points */
  VectorType operator-(const Self & pnt) const;

  /** Add a vector to a point. Return a new point. */
  Self operator+(const VectorType & vec) const;

  /** Subtract a vector from a point. Return a new point. */
  Self operator-(const VectorType & vec) const;

  /** Access an element of a point. */
  VectorType GetVectorFromOrigin() const;

  /** Get a vnl_vector_ref referencing the same memory block */
  vnl_vector_ref< TCoordRep > GetVnlVector();

  /** Get a vnl_vector with a copy of the internal memory block. */
  vnl_vector< TCoordRep > GetVnlVector() const;

  /** Get a vnl_vector_ref referencing the same memory block
   * \deprecated Use GetVnlVector() instead. */
  itkLegacyMacro(vnl_vector_ref< TCoordRep > Get_vnl_vector(void));

  /** Get a vnl_vector with a copy of the internal memory block.
   * \deprecated Use GetVnlVector() instead. */
  itkLegacyMacro(vnl_vector< TCoordRep > Get_vnl_vector(void) const);

  /** Set to median point between the two points
   * given as arguments
   *
   * This method computes:
   *
   * \f[
   *   \overrightarrow{P}=\frac{(\overrightarrow{A}+\overrightarrow{B})}{2}
   * \f]
   *
   * using the two Points given as arguments, and store the result in
   * the Point on which the method is invoked. */
  void SetToMidPoint(const Self &, const Self &);

  /** Set the current point to a barycentric combination of the two points
   * given as arguments.
   *
   * \param A First point
   * \param B Second point
   * \param alpha Weight for the first point
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
   * \sa SetToMedian */
  void SetToBarycentricCombination(const Self & A, const Self & B, double alpha);

  /** Set the current point to a barycentric combination of three points
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
   * If the two weight are \f$ \in [0,1] \f$ , The resulting point will alway be placed
   * inside the triangle formed by the three points given as arguments. */
  void SetToBarycentricCombination(const Self & A, const Self & B, const Self & C,
                                   double weightA,  double weightB);

  /** Set the current point to a barycentric combination of an array of N points
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
   */
  void SetToBarycentricCombination(const Self *P, const double *weights, unsigned int N);

  /** Copy from another Point with a different representation type.
   *  Casting is done with C-Like rules  */
  template< typename TCoordRepB >
  void CastFrom(const Point< TCoordRepB, NPointDimension > & pa)
  {
    for ( unsigned int i = 0; i < NPointDimension; i++ )
      {
      ( *this )[i] = static_cast< TCoordRep >( pa[i] );
      }
  }

  /** Compute the Squared Euclidean Distance from this point to another point
    * with a different representation type.  Casting is done with
    * C-Like rules */

  template< typename TCoordRepB >
  RealType SquaredEuclideanDistanceTo(const Point< TCoordRepB, NPointDimension > & pa) const
  {
    RealType sum = NumericTraits< RealType >::ZeroValue();

    for ( unsigned int i = 0; i < NPointDimension; i++ )
      {
      const RealType component =  static_cast< RealType >( pa[i] );
      const RealType difference = static_cast< RealType >( ( *this )[i] ) - component;
      sum += difference * difference;
      }
    return sum;
  }

  /** Compute the Euclidean Distance from this point to another point
    * with a different representation type.  Casting is done with
    * C-Like rules */
  template< typename TCoordRepB >
  RealType EuclideanDistanceTo(const Point< TCoordRepB, NPointDimension > & pa) const
  {
    const double distance = std::sqrt(
      static_cast< double >( this->SquaredEuclideanDistanceTo(pa) ) );

    return static_cast< RealType >( distance );
  }
};

template< typename T, unsigned int NPointDimension >
std::ostream & operator<<(std::ostream & os,
                                     const Point< T, NPointDimension > & v);

template< typename T, unsigned int NPointDimension >
std::istream & operator>>(std::istream & is,
                                     Point< T, NPointDimension > & v);

/** \class BarycentricCombination
 *  \brief Computes the barycentric combination of an array of N points.
 *
 * This class computes the barycentric combination of an array of N points.
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
 * \ingroup Geometry
 * \ingroup ITKCommon
 */
template< typename TPointContainer, typename TWeightContainer >
class ITK_TEMPLATE_EXPORT BarycentricCombination
{
public:
  /** Convenient typedefs. */
  typedef TPointContainer                      PointContainerType;
  typedef typename PointContainerType::Pointer PointContainerPointer;
  typedef typename PointContainerType::Element PointType;
  typedef TWeightContainer                     WeightContainerType;

  BarycentricCombination() {}
  ~BarycentricCombination() {}

  static PointType Evaluate(
    const PointContainerPointer & points,
    const WeightContainerType & weights);
};
}  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.hxx"
#endif

#endif
