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
#ifndef __itkArray_h
#define __itkArray_h

#include "itkMacro.h"

#include "vxl_version.h"
#if VXL_VERSION_DATE_FULL < 20110428
#error "System installed VXL version is too old. Please make sure the version date is later than 2011-04-28."
#endif

#include "vnl/vnl_vector.h"

namespace itk
{
/** \class Array
 *  \brief Array class with size defined at construction time.
 *
 * This class derives from the vnl_vector<> class.
 * Its size is assigned at construction time (run time) and can
 * not be changed afterwards except by using assignment to another
 * Array.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class Array:
 *
 * - TValueType = Element type stored at each location in the array.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename TValueType >
class Array : public vnl_vector< TValueType >
{
public:

  /** The element type stored at each location in the Array. */
  typedef TValueType                                  ValueType;
  typedef Array                                       Self;
  typedef vnl_vector< TValueType >                    VnlVectorType;
  typedef typename vnl_vector< TValueType>::size_type SizeValueType;

public:

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  Array();

  /** Copy constructor.  Uses VNL copy construtor with correct
   *  setting for memory management.                          */
  Array(const Array&);

  /** Constructor with size. Size can only be changed by assignment */
  explicit Array(SizeValueType dimension);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to
   * that location and it is the user's responsibility to delete it.
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  Array(ValueType *data, SizeValueType sz, bool LetArrayManageMemory = false);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to
   * that location and it is the user's responsibility to delete it.
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  Array(const ValueType *data, SizeValueType sz,
        bool LetArrayManageMemory = false);

  /** Constructor to initialize an array from another of any data type */
  template< class TArrayValue >
  Array(const Array< TArrayValue > & r)
  {
    this->m_LetArrayManageMemory = true;
    this->SetSize( r.GetSize() );
    for( SizeValueType i=0; i<r.GetSize(); i++ )
      {
      this->operator[](i) = static_cast< TValueType >( r[i] );
      }
  }

  /** Set the all the elements of the array to the specified value */
  void Fill(TValueType const & v);

  /** Copy opertor */
  const Self & operator=(const Self & rhs);

  const Self & operator=(const VnlVectorType & rhs);

  /** Return the number of elements in the Array  */
  SizeValueType Size(void) const;
  unsigned int GetNumberOfElements(void) const;

  /** Get one element */
  const ValueType & GetElement(SizeValueType i) const;

  /** Set one element */
  void SetElement(SizeValueType i, const TValueType & value);

  /** Destructively set the size to that given.  Will lose data.  */
  void SetSize(SizeValueType sz);

  SizeValueType GetSize(void) const;

  /** Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType *data, bool LetArrayManageMemory = false);

  /** Similar to the previous method. In the above method, the size must be
   * separately set prior to using user-supplied data. This introduces an
   * unnecessary allocation step to be performed. This method avoids it
   * and should be used to import data wherever possible to avoid this.
   * Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType *data, SizeValueType sz,
               bool LetArrayManageMemory = false);


#ifdef __INTEL_COMPILER
#pragma warning disable 444 //destructor for base class "itk::Array<>" is not virtual
#endif
  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array();

private:

  bool m_LetArrayManageMemory;
};

template< typename TValueType >
std::ostream & operator<<(std::ostream & os, const Array< TValueType > & arr)
{
  os << "[";
  const unsigned int length = arr.size();
  if ( length >= 1 )
    {
    const unsigned int   last   = length - 1;
    for ( unsigned int i = 0; i < last; ++i )
      {
      os << arr[i] << ", ";
      }
      os << arr[last];
    }
  os << "]";
  return os;
}
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray.hxx"
#endif

#endif
