/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  typedef TValueType  ValueType;
  
public:

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  Array(); 

  /** Constructor with size. Size can only be changed by assignment */
  Array(unsigned int dimension);

  /** Set the all the elements of the array to the specified value */
  void Fill (TValueType const& v) { fill(v); }
 
  /** Return the number of elements in the Array  */
  unsigned int Size (void ) const 
      { return static_cast<unsigned int>( this->size() ); }
  unsigned int GetNumberOfElements(void) const 
      { return static_cast<unsigned int>( this->size() ); }

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array() {};
  
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
