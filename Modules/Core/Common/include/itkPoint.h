/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
/**
 * \class Point
 * \brief A templated class holding a geometric point in n-Dimensional space.
 *
 * Point is a templated class that holds a set of coordinates (components).
 * Point can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The VPointDimension defines the number of
 * components in the point array.
 *
 * \ingroup Geometry
 * \ingroup DataRepresentation
 *
 * \sa Image \sa Mesh \sa Vector \sa CovariantVector \sa Matrix
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/DistanceBetweenPoints,Distance between two points}
 * \sphinxexample{Core/Common/DistanceBetweenIndices,Distance between two indices}
 * \endsphinx
 */
template <typename TCoordRep, unsigned int VPointDimension = 3>
class ITK_TEMPLATE_EXPORT Point : public FixedArray<TCoordRep, VPointDimension>
{
public:
  /** Standard class type aliases. */
  using Self = Point;
  using Superclass = FixedArray<TCoordRep, VPointDimension>;

  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.   */
  using ValueType = TCoordRep;
  using CoordRepType = TCoordRep;

  using RealType = typename NumericTraits<ValueType>::RealType;

  /** Dimension of the Space */
  static constexpr unsigned int PointDimension = VPointDimension;

  /** The Array type from which this Vector is derived. */
  using BaseArray = FixedArray<TCoordRep, VPointDimension>;
  using Iterator = typename BaseArray::Iterator;
  using ConstIterator = typename BaseArray::ConstIterator;

  /** Get the dimension (size) of the point. */
  static unsigned int
  GetPointDimension()
  {
    return VPointDimension;
  }

  /** VectorType define the difference between two Points */
  using VectorType = Vector<ValueType, VPointDimension>;

  /** Default-constructor.
   * \note The other five "special member functions" are defaulted implicitly, following the C++ "Rule of Zero". */
  Point() = default;

  /** Pass-through constructors for different type points. */
  template <typename TPointValueType>
  Point(const Point<TPointValueType, VPointDimension> & r)
    : BaseArray(r)
  {}
  /** Pass-through constructors for plain arrays. */
  template <typename TPointValueType>
  Point(const TPointValueType r[VPointDimension])
    : BaseArray(r)
  {}
  Point(const ValueType r[VPointDimension])
    : BaseArray(r)
  {}

#if defined(ITK_LEGACY_REMOVE)
  /** Prevents copy-initialization from `nullptr`, as well as from `0` (NULL). */
  Point(std::nullptr_t) = delete;

  /** Explicit constructors for single values */
  template <typename TPointValueType>
  explicit Point(const TPointValueType & v)
    : BaseArray(v)
  {}
  explicit Point(const ValueType & v)
    : BaseArray(v)
  {}
#else
  /** Pass-through constructors for single values
   * \note ITK_LEGACY_REMOVE=ON will disallow implicit conversion from a single value. */
  template <typename TPointValueType>
  Point(const TPointValueType & v)
    : BaseArray(v)
  {}
  Point(const ValueType & v)
    : BaseArray(v)
  {}
#endif

  /** Explicit constructor for std::array. */
  explicit Point(const std::array<ValueType, VPointDimension> & stdArray)
    : BaseArray(stdArray)
  {}

  /** Pass-through assignment operator for a plain array. */
  Point &
  operator=(const ValueType r[VPointDimension]);

  /** Compare two points for equality. */
  bool
  operator==(const Self & pt) const
  {
    bool same = true;

    for (unsigned int i = 0; i < VPointDimension && same; ++i)
    {
      same = (Math::ExactlyEquals((*this)[i], pt[i]));
    }
    return same;
  }

  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self);

  /** Point operator+=.  Adds a vector to the current point. */
  const Self &
  operator+=(const VectorType & vec);

  /** Point operator-=.  Subtracts a vector from a current point. */
  const Self &
  operator-=(const VectorType & vec);

  /** Computes the Vector difference between two points */
  VectorType
  operator-(const Self & pnt) const;

  /** Add a vector to a point. Return a new point. */
  Self
  operator+(const VectorType & vec) const;

  /** Subtract a vector from a point. Return a new point. */
  Self
  operator-(const VectorType & vec) const;

  /** Access an element of a point. */
  VectorType
  GetVectorFromOrigin() const;

  /** Get a vnl_vector_ref referencing the same memory block */
  vnl_vector_ref<TCoordRep>
  GetVnlVector();

  /** Get a vnl_vector with a copy of the internal memory block. */
  vnl_vector<TCoordRep>
  GetVnlVector() const;

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
  void
  SetToMidPoint(const Self &, const Self &);

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
  void
  SetToBarycentricCombination(const Self & A, const Self & B, double alpha);

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
   * If the two weight are \f$ \in [0,1] \f$ , The resulting point will always be placed
   * inside the triangle formed by the three points given as arguments. */
  void
  SetToBarycentricCombination(const Self & A, const Self & B, const Self & C, double weightForA, double weightForB);

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
  void
  SetToBarycentricCombination(const Self * P, const double * weights, unsigned int N);

  /** Copy from another Point with a different representation type.
   *  Casting is done with C-Like rules  */
  template <typename TCoordRepB>
  void
  CastFrom(const Point<TCoordRepB, VPointDimension> & pa)
  {
    for (unsigned int i = 0; i < VPointDimension; ++i)
    {
      (*this)[i] = static_cast<TCoordRep>(pa[i]);
    }
  }

  /** Compute the Squared Euclidean Distance from this point to another point
   * with a different representation type.  Casting is done with
   * C-Like rules */

  template <typename TCoordRepB>
  RealType
  SquaredEuclideanDistanceTo(const Point<TCoordRepB, VPointDimension> & pa) const
  {
    RealType sum = NumericTraits<RealType>::ZeroValue();

    for (unsigned int i = 0; i < VPointDimension; ++i)
    {
      const auto     component = static_cast<RealType>(pa[i]);
      const RealType difference = static_cast<RealType>((*this)[i]) - component;
      sum += difference * difference;
    }
    return sum;
  }

  /** Compute the Euclidean Distance from this point to another point
   * with a different representation type.  Casting is done with
   * C-Like rules */
  template <typename TCoordRepB>
  RealType
  EuclideanDistanceTo(const Point<TCoordRepB, VPointDimension> & pa) const
  {
    const double distance = std::sqrt(static_cast<double>(this->SquaredEuclideanDistanceTo(pa)));

    return static_cast<RealType>(distance);
  }
};

template <typename T, unsigned int VPointDimension>
std::ostream &
operator<<(std::ostream & os, const Point<T, VPointDimension> & vct);

template <typename T, unsigned int VPointDimension>
std::istream &
operator>>(std::istream & is, Point<T, VPointDimension> & vct);

/**
 * \class BarycentricCombination
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
template <typename TPointContainer, typename TWeightContainer>
class ITK_TEMPLATE_EXPORT BarycentricCombination
{
public:
  /** Convenient type alias. */
  using PointContainerType = TPointContainer;
  using PointContainerPointer = typename PointContainerType::Pointer;
  using PointType = typename PointContainerType::Element;
  using WeightContainerType = TWeightContainer;

  BarycentricCombination() = default;
  ~BarycentricCombination() = default;

  static PointType
  Evaluate(const PointContainerPointer & points, const WeightContainerType & weights);
};


template <typename TCoordRep, unsigned int VPointDimension>
inline void
swap(Point<TCoordRep, VPointDimension> & a, Point<TCoordRep, VPointDimension> & b)
{
  a.swap(b);
}


/** Makes a Point object, having the specified values as coordinates. */
template <typename TValue, typename... TVariadic>
auto
MakePoint(const TValue firstValue, const TVariadic... otherValues)
{
  // Assert that the other values have the same type as the first value.
  const auto assertSameType = [](const auto value) {
    static_assert(std::is_same<decltype(value), const TValue>::value, "Each value must have the same type!");
    return true;
  };
  const bool assertions[] = { true, assertSameType(otherValues)... };
  (void)assertions;
  (void)assertSameType;

  constexpr unsigned int              dimension{ 1 + sizeof...(TVariadic) };
  const std::array<TValue, dimension> stdArray{ { firstValue, otherValues... } };
  return Point<TValue, dimension>{ stdArray };
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPoint.hxx"
#endif

#endif
