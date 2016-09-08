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
#ifndef itkSize_h
#define itkSize_h

#include "itkMacro.h"
#include "itkIntTypes.h"

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
template< unsigned int VDimension = 2 >
class Size
{
public:
  /** Standard class typedefs. */
  typedef Size Self;

  /** Compatible Size and value typedef */
  typedef   Size< VDimension > SizeType;
  typedef   itk::SizeValueType SizeValueType;

  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VDimension);

  /** Get the dimension of the size object. */
  static unsigned int GetSizeDimension(void) { return VDimension; }

  /** Add two sizes.  */
  const Self
  operator+(const Self & vec) const
  {
    Self result;

    for ( unsigned int i = 0; i < VDimension; i++ )
          { result[i] = m_Size[i] + vec.m_Size[i]; }
    return result;
  }

  /** Increment size by a size.  */
  const Self &
  operator+=(const Self & vec)
  {
    for ( unsigned int i = 0; i < VDimension; i++ )
          { m_Size[i] += vec.m_Size[i]; }
    return *this;
  }

  /** Subtract two sizes.  */
  const Self
  operator-(const Self & vec) const
  {
    Self result;

    for ( unsigned int i = 0; i < VDimension; i++ )
          { result[i] = m_Size[i] - vec.m_Size[i]; }
    return result;
  }

  /** Decrement size by a size.  */
  const Self &
  operator-=(const Self & vec)
  {
    for ( unsigned int i = 0; i < VDimension; i++ )
          { m_Size[i] -= vec.m_Size[i]; }
    return *this;
  }

  /** Multiply two sizes (elementwise product).  */
  const Self
  operator*(const Self & vec) const
  {
    Self result;

    for ( unsigned int i = 0; i < VDimension; i++ )
          { result[i] = m_Size[i] * vec.m_Size[i]; }
    return result;
  }

  /** Multiply two sizes (elementwise product).  */
  const Self &
  operator*=(const Self & vec)
  {
    for ( unsigned int i = 0; i < VDimension; i++ )
          { m_Size[i] *= vec.m_Size[i]; }
    return *this;
  }

  /** Compare two sizes. */
  bool
  operator==(const Self & vec) const
  {
    bool same = 1;

    for ( unsigned int i = 0; i < VDimension && same; i++ )
          { same = ( m_Size[i] == vec.m_Size[i] ); }
    return same;
  }

  /** Compare two sizes. */
  bool
  operator!=(const Self & vec) const
  {
    bool same = 1;

    for ( unsigned int i = 0; i < VDimension && same; i++ )
          { same = ( m_Size[i] == vec.m_Size[i] ); }
    return !same;
  }

  /** Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. No bounds checking is performed. */
  SizeValueType & operator[](unsigned int dim)
  { return m_Size[dim]; }

  /** Access an element of the size. Elements are numbered
   * 0, ..., VDimension-1. This version can only be an rvalue.
   * No bounds checking is performed. */
  SizeValueType operator[](unsigned int dim) const
  { return m_Size[dim]; }

  /** Get the size. This provides a read only reference to the size.
   * \sa SetSize */
  const SizeValueType * GetSize() const { return m_Size; }

  /** Set the size.
   * Try to prototype this function so that val has to point to a block of
   * memory that is the appropriate size. \sa GetSize */
  void SetSize(const SizeValueType val[VDimension])
  {
    std::copy(val,
              val+VDimension,
              m_Size);
  }

  /** Set an element of the Size.
   * sets the value of one of the elements in the Size
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed.
   * \sa SetSize() \sa GetElement() */
  void SetElement(unsigned long element, SizeValueType val)
  { m_Size[element] = val;  }

  /** Get an element of the Size.
   * gets the value of one of the elements in the size
   * This method is mainly intended to facilitate the access to elements
   * from Tcl and Python where C++ notation is not very convenient.
   * \warning No bound checking is performed
   * \sa GetSize() \sa SetElement() */
  SizeValueType GetElement(unsigned long element) const
  { return m_Size[element]; }

  /** Set one value for the index in all dimensions.  Useful for initializing
   * an offset to zero. */
  void Fill(SizeValueType value)
  { for ( unsigned int i = 0; i < VDimension; ++i ) { m_Size[i] = value; } }

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

#if defined( ITK_WRAPPING_PARSER )
  // Do not use c++11 'delete' keyword here.  This code block is here to
  // explicitly provide the wrapping facilities with handles to the default and
  // copy constructors, and the assignment operator that are otherwise declared
  // implicitly.
  Size();
  Size(const Self&);
  void operator=(const Self&);
#endif
};

template< unsigned int VDimension >
std::ostream & operator<<(std::ostream & os, const Size< VDimension > & size)
{
  os << "[";
  for ( unsigned int i = 0; i + 1 < VDimension; ++i )
    {
    os << size[i] << ", ";
    }
  if ( VDimension >= 1 )
    {
    os << size[VDimension - 1];
    }
  os << "]";
  return os;
}
} // end namespace itk

#endif
