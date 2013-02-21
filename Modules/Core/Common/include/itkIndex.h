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
#ifndef __itkIndex_h
#define __itkIndex_h

#include "itkOffset.h"
#include "itkFixedArray.h"
#include "itkMath.h"

#include <memory>


namespace itk
{
namespace Functor
{
template< unsigned int VIndexDimension >
class IndexLexicographicCompare;
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
class Index
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
  static unsigned int GetIndexDimension();

  /** Compatible Size typedef. */
  typedef   Size<VIndexDimension> SizeType;

  /** Compatible Offset and Offset value typedef. */
  typedef   Offset<VIndexDimension> OffsetType;
  typedef ::itk::OffsetValueType     OffsetValueType;

  /** Lexicographic ordering functor type.  */
  typedef Functor::IndexLexicographicCompare<VIndexDimension> LexicographicCompare;

  /** Add a size to an index. This method models a random access Index. */
  const Self operator+(const SizeType & size) const;

  /** Increment index by a size. This method models a random access Index. */
  const Self & operator+=(const SizeType & size);

  /** Subtract a size from an index. This method models a random access Index.  */
  const Self operator-(const SizeType & size) const;

  /** Decrement index by a size. This method models a random access Index. */
  const Self & operator-=(const SizeType & size);

  /** Add an offset to an index. */
  const Self operator+(const OffsetType & offset) const;

  /** Increment index by an offset. This method models a random access Index. */
  const Self & operator+=(const OffsetType & offset);

  /** Decrement index by an offset. This method models a random access Index. */
  const Self & operator-=(const OffsetType & offset);

  /** Subtract an offset from an index. */
  const Self operator-(const OffsetType & off) const;

  /** Subtract two indices. This method models a random access Index. */
  const OffsetType operator-(const Self & vec) const;

  /** Multiply an index by a size (elementwise product). This method
   * models a random access Index.  */
  const Self operator*(const SizeType & vec) const;

  /** Compare two indices. */
  bool operator==(const Self & vec) const;

  /** Compare two indices. */
  bool operator!=(const Self & vec) const;

  /** Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. No bounds checking is performed. */
  IndexValueType & operator[](const unsigned int dim);

  /** Access an element of the index. Elements are numbered
   * 0, ..., VIndexDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  IndexValueType operator[](const unsigned int dim) const;

  /** Get the index. This provides a read only reference to the index.
   * \sa SetIndex() */
  const IndexValueType * GetIndex() const;

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetIndex() */
  void SetIndex(const IndexValueType val[VIndexDimension]);

  /** Sets the value of one of the elements in the index.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa SetIndex()
   * \sa GetElement() */
  void SetElement(const unsigned long element, const IndexValueType val);

  /** Gets the value of one of the elements in the index.
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetIndex()
   * \sa SetElement() */
  IndexValueType GetElement(const unsigned long element) const;

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VIndexDimension-1. */
  static Self GetBasisIndex(const unsigned int dim);

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(const IndexValueType value);

  /** Index is an "aggregate" class.  Its data is public (m_Index)
   * allowing for fast and convenient instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Index<3> index = {5, 2, 7}; */
  IndexValueType m_Index[VIndexDimension];

  /** Copy values from a FixedArray by rounding each one of the components */
  template <class TCoordRep>
  inline void CopyWithRound(const FixedArray<TCoordRep, VIndexDimension> & point)
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
  template <class TCoordRep>
  inline void CopyWithCast(const FixedArray<TCoordRep, VIndexDimension> & point)
  {
    for( unsigned int i = 0; i < VIndexDimension; ++i )
      {
      m_Index[i] = static_cast<IndexValueType>( point[i] );
      }
  }

// force gccxml to find the constructors found before the internal upgrade to
// gcc 4.2
#if defined( CABLE_CONFIGURATION )
  Index();                      // purposely not implemented
  Index(const Self &);          // purposely not implemented
  void operator=(const Self &); // purposely not implemented

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
template <unsigned int VIndexDimension>
class IndexLexicographicCompare
{
public:
  bool operator()(Index<VIndexDimension> const & l, Index<VIndexDimension> const & r) const;

};
}

} // end namespace itk

extern template class itk::Index<2u>;
extern template class itk::Index<3u>;
extern template class itk::Index<4u>;
extern template class itk::Functor::IndexLexicographicCompare<2u>;
extern template class itk::Functor::IndexLexicographicCompare<3u>;
extern template class itk::Functor::IndexLexicographicCompare<4u>;

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIndex.hxx"
#endif

#endif
