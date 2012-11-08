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
#ifndef __itkOffset_h
#define __itkOffset_h

#include "itkSize.h"

#include <memory>

namespace itk
{
namespace Functor
{
template< unsigned int VOffsetDimension >
class OffsetLexicographicCompare;
}

/**
 * \class Offset
 * \brief Represent the offset between two n-dimensional indexes
 *  in a n-dimensional image.
 *
 * Offset is a templated class to represent a multi-dimensional offset,
 * i.e. (i,j,k,...). Offset is templated over the dimension of the space.
 *
 * For the sake of efficiency, Offset does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * \sa Index
 * \ingroup ImageAccess
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/Offset,Add an offset to a pixel index}
 * \endwiki
 */

template< unsigned int VOffsetDimension = 2 >
class Offset
{
public:
  /** Standard class typedefs. */
  typedef Offset Self;

  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VOffsetDimension);

  /** Get the dimension (size) of the index. */
  static unsigned int GetOffsetDimension() { return VOffsetDimension; }

  /** Compatible offset typedefs. */
  typedef   Offset< VOffsetDimension > OffsetType;
  typedef   itk::OffsetValueType       OffsetValueType;

  /** Lexicographic ordering functor type.  */
  typedef Functor::OffsetLexicographicCompare< VOffsetDimension > LexicographicCompare;

  /** Add an offset to an offset. */
  const Self operator+(const Self & offset) const;

  /** Add a size to an offset.  */
  const Self operator+(const Size< VOffsetDimension > & size) const;

  /** Increment index by a size.  */
  const Self & operator+=(const Size< VOffsetDimension > & size);

  /** Decrement index by a size.  */
  const Self & operator-=(const Size< VOffsetDimension > & size);

  /** Subtract two offsets. */
  const Self operator-(const Self & vec);

  /** Increment offset by an offset.  */
  const Self & operator+=(const Self & vec);

  /** Decrement offset by an offset.  */
  const Self & operator-=(const Self & vec);

  /** Compare two offsets. */
  bool operator==(const Self & vec) const;

  /** Compare two offsets. */
  bool operator!=(const Self & vec) const;

  /** Access an element of the offset. Elements are numbered
   * 0, ..., VOffsetDimension-1. No bounds checking is performed. */
  inline OffsetValueType & operator[](unsigned int dim)
  { return m_Offset[dim]; }

  /** Access an element of the index. Elements are numbered
   * 0, ..., VOffsetDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  inline OffsetValueType operator[](unsigned int dim) const
  { return m_Offset[dim]; }

  /** Get the index. This provides a read only reference to the index.
   * \sa SetOffset() */
  inline const OffsetValueType * GetOffset() const { return m_Offset; }

  /** Set the index.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size.
   * \sa GetOffset() */
  void SetOffset(const OffsetValueType val[VOffsetDimension]);

  /** Return a basis vector of the form [0, ..., 0, 1, 0, ... 0] where the "1"
   * is positioned in the location specified by the parameter "dim". Valid
   * values of "dim" are 0, ..., VOffsetDimension-1. */
  static Self GetBasisOffset(unsigned int dim);

  /** Set one value for the offset in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(OffsetValueType value);

  /** Offset is an "aggregate" class.  Its data is public (m_Offset)
   * allowing for fast and convenient instantiations/assignments.
   *
   * The following syntax for assigning an index is allowed/suggested:
   *    Offset<3> index = {5, 2, 7}; */
  OffsetValueType m_Offset[VOffsetDimension];

// force gccxml to find the constructors found before the internal upgrade to
// gcc 4.2
#if defined( CABLE_CONFIGURATION )
  Offset();                     //purposely not implemented
  Offset(const Self &);         //purposely not implemented
  void operator=(const Self &); //purposely not implemented

#endif
};

namespace Functor
{
/** \class OffsetLexicographicCompare
 * \brief Order Offset instances lexicographically.
 *
 * This is a comparison functor suitable for storing Offset instances
 * in an STL container.  The ordering is total and unique but has
 * little geometric meaning.
 * \ingroup ITKCommon
 */
template< unsigned int VOffsetDimension >
class OffsetLexicographicCompare
{
public:
  bool operator()(Offset< VOffsetDimension > const & l,
                  Offset< VOffsetDimension > const & r) const;
};
}


template< unsigned int VOffsetDimension >
std::ostream & operator<<(std::ostream & os, const Offset< VOffsetDimension > & ind)
{
  os << "[";
  unsigned int dimlim = VOffsetDimension - 1;
  for ( unsigned int i = 0; i < dimlim; ++i )
    {
    os << ind[i] << ", ";
    }
  if ( VOffsetDimension >= 1 )
    {
    os << ind[VOffsetDimension - 1];
    }
  os << "]";
  return os;
}

} // end namespace itk

extern template class itk::Offset<1u>;
extern template class itk::Offset<2u>;
extern template class itk::Offset<3u>;
extern template class itk::Offset<4u>;
extern template class itk::Offset<5u>;
extern template class itk::Offset<6u>;
extern template class itk::Functor::OffsetLexicographicCompare<1u>;
extern template class itk::Functor::OffsetLexicographicCompare<2u>;
extern template class itk::Functor::OffsetLexicographicCompare<3u>;
extern template class itk::Functor::OffsetLexicographicCompare<4u>;
extern template class itk::Functor::OffsetLexicographicCompare<5u>;
extern template class itk::Functor::OffsetLexicographicCompare<6u>;

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOffset.hxx"
#endif

#endif
