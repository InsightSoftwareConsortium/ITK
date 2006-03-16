/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkArray_h
#define __itkArray_h

#include "itkMacro.h"
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
 */
template <typename TValueType >
class Array : public vnl_vector< TValueType >
{
public:
 
  /** The element type stored at each location in the Array. */
  typedef TValueType             ValueType;
  typedef Array                  Self;
  typedef vnl_vector<TValueType> VnlVectorType;
  
public:

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  Array(); 

  /** Constructor with size. Size can only be changed by assignment */
  Array(unsigned int dimension);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to 
   * that location and it is the user's responsibility to delete it. 
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */ 
  Array( ValueType* data, unsigned int sz, bool LetArrayManageMemory = false);
  
  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to 
   * that location and it is the user's responsibility to delete it. 
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */ 
  Array( const ValueType* data, unsigned int sz, 
         bool LetArrayManageMemory = false);


  /** Set the all the elements of the array to the specified value */
  void Fill (TValueType const& v) { fill(v); }

  /** Copy opertor */
  const Self & operator=( const Self &rhs );
  const Self & operator=( const VnlVectorType & rhs );
 
  /** Return the number of elements in the Array  */
  unsigned int Size (void ) const 
      { return static_cast<unsigned int>( this->size() ); }
  unsigned int GetNumberOfElements(void) const 
      { return static_cast<unsigned int>( this->size() ); }

  /** Get one element */
  const TValueType & GetElement( unsigned int i ) const
    { return this->operator[]( i ); }

  /** Set one element */
  void SetElement( unsigned int i, const TValueType & value )
    { this->operator[]( i ) = value; }

  /** Destructively set the size to that given.  Will lose data.  */
  void SetSize(unsigned int sz);
  unsigned int GetSize(void) const 
      { return static_cast<unsigned int>( this->size() ); }

  /** Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType* data,bool LetArrayManageMemory = false);

  /** Similar to the previous method. In the above method, the size must be 
   * seperately set prior to using user-supplied data. This introduces an
   * unnecessary allocation step to be performed. This method avoids it 
   * and should be used to import data whereever possible to avoid this.
   * Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType* data, unsigned int sz, 
               bool LetArrayManageMemory = false);

  
  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array();

private:

  bool m_LetArrayManageMemory;
  
};


template <typename TValueType >
std::ostream & operator<<(std::ostream &os, const Array<TValueType> &arr)
{
  const unsigned int length = arr.size();
  const signed int last   = (unsigned int) length - 1;

  os << "[";
  for (signed int i=0; i < last; ++i)
    {
    os << arr[i] << ", ";
    }
  if (length >= 1)
    {
    os << arr[last];
    }
  os << "]";
  return os;
}

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray.txx"
#endif


#endif
