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
#ifndef itkIndex_h
#define itkIndex_h

#include "itkOffset.h"
#include "itkFixedArray.h"
#include "itkMath.h"

#include <memory>

namespace itk
{
namespace Functor
{
// Forward reference because of circular dependencies
template< unsigned int VIndexDimension >
class ITK_TEMPLATE_EXPORT IndexLexicographicCompare;
}

/** \class Index
 * \brief Represent a n-dimensional index in a n-dimensional image.
 *
 * Index is a templated class to represent a multi-dimensional index,
 * i.e. (i,j,k,...). Index is templated over the dimension of the index.
 * ITK assumes the first element of an index is the fastest moving index.
 *
 * For efficiency sake, Index does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Index is an "aggregate" class.  Its data is public (m_Index)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
 *    Index<3> index = {{5, 2, 7}};
 *
 * The double braces {{ and }} are needed to prevent a compiler warning
 * about a partly bracketed initializer.
 *
 * \remark
 * Should there be an itkBoundedIndex to handle bounds checking? Or should
 * there be an API to perform bounded increments in the iterator.
 *
 * \ingroup ImageAccess
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/DistanceBetweenIndices,Distance between two indices}
 * \wikiexample{Images/Index,An object which holds the index of a pixel}
 * \endwiki
 */

template< unsigned int VIndexDimension = 2 >
class ITK_TEMPLATE_EXPORT Index
{
public:
  /** Standard class typedefs. */
  typedef Index Self;

  /** Compatible Index and value typedef */
  typedef   Index< VIndexDimension > IndexType;
  typedef   ::itk::IndexValueType    IndexValueType;

  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VIndexDimension);

  /** Get the dimension (size) of the index. */
  static unsigned int GetIndexDimension() { return VIndexDimension; }

  /** Compatible Size typedef. */
  typedef   Size< VIndexDimension > SizeType;

  /** Compatible Offset and Offset value typedef. */
  typedef   Offset< VIndexDimension >            OffsetType;
  typedef   ::itk::OffsetValueType               OffsetValueType;

  /** Lexicographic ordering functor type.  */
  typedef Functor::IndexLexicographicCompare< VIndexDimension > LexicographicCompare;

  /** Add a size to an index. This method models a random access Index. */
  const Self
  operator+(const SizeType & size) const
  {
    Self result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] + static_cast< IndexValueType >( size[i] ); }
    return result;
  }

  /** Increment index by a size. This method models a random access Index. */
  const Self &
  operator+=(const SizeType & size)
  {
    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { m_Index[i] += static_cast< IndexValueType >( size[i] ); }
    return *this;
  }

  /** Subtract a size from an index. This method models a random access Index.
    */
  const Self
  operator-(const SizeType & size) const
  {
    Self result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] - static_cast< IndexValueType >( size[i] ); }
    return result;
  }

  /** Decrement index by a size. This method models a random access Index. */
  const Self &
  operator-=(const SizeType & size)
  {
    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { m_Index[i] -= static_cast< IndexValueType >( size[i] ); }
    return *this;
  }

  /** Add an offset to an index. */
  const Self
  operator+(const OffsetType & offset) const
  {
    Self result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] + offset[i]; }
    return result;
  }

  /** Increment index by an offset. This method models a random access Index. */
  const Self &
  operator+=(const OffsetType & offset)
  {
    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { m_Index[i] += offset[i]; }
    return *this;
  }

  /** Decrement index by an offset. This method models a random access Index. */
  const Self &
  operator-=(const OffsetType & offset)
  {
    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { m_Index[i] -= offset[i]; }
    return *this;
  }

  /** Subtract an offset from an index. */
  const Self
  operator-(const OffsetType & off) const
  {
    Self result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] - off.m_Offset[i]; }
    return result;
  }

  /** Subtract two indices. This method models a random access Index. */
  const OffsetType
  operator-(const Self & vec) const
  {
    OffsetType result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] - vec.m_Index[i]; }
    return result;
  }

  /** Multiply an index by a size (elementwise product). This method
   * models a random access Index.  */
  const Self
  operator*(const SizeType & vec) const
  {
    Self result;

    for ( unsigned int i = 0; i < VIndexDimension; i++ )
                { result[i] = m_Index[i] * static_cast< IndexValueType >( vec.m_Size[i] ); }
    return result;
  }

  /** Compare two indices. */
  bool
  operator==(const Self & vec) const
  {
    bool same = true;

    for ( unsigned int i = 0; i < VIndexDimension && same; i++ )
                { same = ( m_Index[i] == vec.m_Index[i] ); }
    return same;
  }

  /** Compare two indices. */
  bool
  operator!=(const Self & vec) const
  {
    bool same = true;

    for ( unsigned int i = 0; i < VIndexDimension && same; i++ )
                { same = ( m_Index[i] == vec.m_Index[i] ); }
    return !same;
  }

// false positive warnings with GCC 4.9
#if defined( __GNUC__ )
#if ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ == 9 )
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
#endif
  /** Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. No bounds checking is performed. */
  IndexValueType & operator[](unsigned int dim)
  {
    return m_Index[dim];
  }
#if defined( __GNUC__ )
#if ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ == 9 )
#pragma GCC diagnostic pop
#endif
#endif

  /** Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  IndexValueType operator[](unsigned int dim) const
  { return m_Index[dim]; }

  /** Get the index. This provides a read only reference to the index.
   * \sa SetIndex() */
  const IndexValueType * GetIndex() const { return m_Index; }

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetIndex() */
  void SetIndex(const IndexValueType val[VIndexDimension])
  {
    std::copy(val,
              val+VIndexDimension,
              m_Index);
  }

  /** Sets the value of one of the elements in the index.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa SetIndex()
   * \sa GetElement() */
  void SetElement(unsigned long element, IndexValueType val)
  { m_Index[element] = val;  }

  /** Gets the value of one of the elements in the index.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetIndex()
   * \sa SetElement() */
  IndexValueType GetElement(unsigned long element) const
  { return m_Index[element]; }

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VIndexDimension-1. */
  static Self GetBasisIndex(unsigned int dim);

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(IndexValueType value)
  { for ( unsigned int i = 0; i < VIndexDimension; ++i ) { m_Index[i] = value; } }

  /** Index is an "aggregate" class.  Its data is public (m_Index)
   * allowing for fast and convenient instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Index<3> index = {5, 2, 7}; */
  IndexValueType m_Index[VIndexDimension];

  /** Copy values from a FixedArray by rounding each one of the components */
  template< typename TCoordRep >
  inline void CopyWithRound(const FixedArray< TCoordRep, VIndexDimension > & point)
  {
    itkForLoopRoundingAndAssignmentMacro(IndexType,
                                         ContinuousIndexType,
                                         IndexValueType,
                                         m_Index,
                                         point,
                                         VIndexDimension);
    /* NON TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING data version
     * Leaving here for documentation purposes
     * for ( unsigned int i = 0; i < VIndexDimension; ++i )
     *   {
     *   m_Index[i] = Math::Round< IndexValueType >(point[i]);
     *   }
     */
  }

  /** Copy values from a FixedArray by casting each one of the components */
  template< typename TCoordRep >
  inline void CopyWithCast(const FixedArray< TCoordRep, VIndexDimension > & point)
  {
    for ( unsigned int i = 0; i < VIndexDimension; ++i )
      {
      m_Index[i] = static_cast< IndexValueType >( point[i] );
      }
  }

#if defined( ITK_WRAPPING_PARSER )
  // Do not use c++11 'delete' keyword here.  This code block is here to
  // explicitly provide the wrapping facilities with handles to the default and
  // copy constructors, and the assignment operator that are otherwise declared
  // implicitly.
  Index();
  Index(const Self&);
  void operator=(const Self&);
#endif
};

namespace Functor
{
/** \class IndexLexicographicCompare
 * \brief Order Index instances lexicographically.
 *
 * This is a comparison functor suitable for storing Index instances
 * in an STL container.  The ordering is total and unique but has
 * little geometric meaning.
 * \ingroup ITKCommon
 */
template< unsigned int VIndexDimension >
class ITK_TEMPLATE_EXPORT IndexLexicographicCompare
{
public:
  bool operator()(Index< VIndexDimension > const & l,
                  Index< VIndexDimension > const & r) const
  {
    for ( unsigned int i = 0; i < VIndexDimension; ++i )
      {
      if ( l.m_Index[i] < r.m_Index[i] )
        {
        return true;
        }
      else if ( l.m_Index[i] > r.m_Index[i] )
        {
        return false;
        }
      }
    return false;
  }
};
}

template< unsigned int VIndexDimension >
Index< VIndexDimension >
Index< VIndexDimension >
::GetBasisIndex(unsigned int dim)
{
  Self ind;

  memset(ind.m_Index, 0, sizeof( IndexValueType ) * VIndexDimension);
  ind.m_Index[dim] = 1;
  return ind;
}

template< unsigned int VIndexDimension >
std::ostream & operator<<(std::ostream & os, const Index< VIndexDimension > & ind)
{
  os << "[";
  for ( unsigned int i = 0; i + 1 < VIndexDimension; ++i )
    {
    os << ind[i] << ", ";
    }
  if ( VIndexDimension >= 1 )
    {
    os << ind[VIndexDimension - 1];
    }
  os << "]";
  return os;
}
} // end namespace itk

#endif
