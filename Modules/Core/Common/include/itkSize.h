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
#ifndef __itkSize_h
#define __itkSize_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include <cstring>

namespace itk
{
/** \class Size
 * \brief Represent the size (bounds) of a n-dimensional image.
 *
 * Size is a class to represent multi-dimensional array bounds,
 * templated over the dimension.  Insight assumes that the first
 * element of Size is the fastest moving index.
 *
 * For the sake of efficiency, Size does not define a default constructor, a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Size is an "aggregate" class.  Its data is public (m_Size)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning a size is allowed/suggested:
 *    Size<3> size = {256, 256, 20};
 *
 * \sa Index
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Images/Size,An object which holds the size of an image}
 * \endwiki
 */
template <unsigned int VDimension = 2>
class Size
{
public:
  /** Standard class typedefs. */
  typedef Size Self;

  /** Compatible Size and value typedef */
  typedef   Size<VDimension>   SizeType;
  typedef   itk::SizeValueType SizeValueType;

  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VDimension);

  /** Get the dimension of the size object. */
  static unsigned int GetSizeDimension(void)
  {
    return VDimension;
  }

  /** Add two sizes.  */
  const Self operator+(const Self & vec) const;

  /** Increment size by a size.  */
  const Self & operator+=(const Self & vec);

  /** Subtract two sizes.  */
  const Self operator-(const Self & vec) const;

  /** Decrement size by a size.  */
  const Self & operator-=(const Self & vec);

  /** Multiply two sizes (elementwise product).  */
  const Self operator*(const Self & vec) const;

  /** Multiply two sizes (elementwise product).  */
  const Self & operator*=(const Self & vec);

  /** Compare two sizes. */
  bool operator==(const Self & vec) const;

  /** Compare two sizes. */
  bool operator!=(const Self & vec) const;

  /** Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. No bounds checking is performed. */
  inline SizeValueType & operator[](unsigned int dim)
  {
    return m_Size[dim];
  }

  /** Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  inline SizeValueType operator[](unsigned int dim) const
  {
    return m_Size[dim];
  }

  /** Get the size. This provides a read only reference to the size.
   * \sa SetSize */
  inline const SizeValueType * GetSize() const
  {
    return m_Size;
  }

  /** Set the size.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size. \sa GetSize */
  inline void SetSize(const SizeValueType val[VDimension])
  {
    memcpy(m_Size, val, sizeof( SizeValueType ) * VDimension);
  }

  /** Set an element of the Size.
   * sets the value of one of the elements in the Size
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed.
   * \sa SetSize() \sa GetElement() */
  inline void SetElement(unsigned long element, SizeValueType val)
  {
    m_Size[element] = val;
  }

  /** Get an element of the Size.
   * gets the value of one of the elements in the size
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetSize() \sa SetElement() */
  inline SizeValueType GetElement(unsigned long element) const
  {
    return m_Size[element];
  }

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(SizeValueType value);

  /** Size is an "aggregate" class.  Its data is public (m_Size)
   * allowing for fast and convenient instantiations/assignments.
   *
   * The following syntax for assigning a size is allowed/suggested:
   *
   * Size<3> size = {{256, 256, 20}};
   *
   * The doubled braces {{ and }} are required to prevent `gcc -Wall'
   * (and perhaps other compilers) from complaining about a partly
   * bracketed initializer. */
  SizeValueType m_Size[VDimension];

// force gccxml to find the constructors found before the internal upgrade to
// gcc 4.2
#if defined( CABLE_CONFIGURATION )
  Size();                       // purposely not implemented
  Size(const Self &);           // purposely not implemented
  void operator=(const Self &); // purposely not implemented

#endif
};

template <unsigned int VDimension>
std::ostream & operator<<(std::ostream & os, const Size<VDimension> & size)
{
  os << "[";
  for( unsigned int i = 0; i + 1 < VDimension; ++i )
    {
    os << size[i] << ", ";
    }
  if( VDimension >= 1 )
    {
    os << size[VDimension - 1];
    }
  os << "]";
  return os;
}

} // end namespace itk

extern template class itk::Size<1u>;
extern template class itk::Size<2u>;
extern template class itk::Size<3u>;
extern template class itk::Size<4u>;
extern template class itk::Size<5u>;
extern template class itk::Size<6u>;

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSize.hxx"
#endif

#endif
