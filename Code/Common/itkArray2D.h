/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkArray2D_h
#define __itkArray2D_h

#include "itkMacro.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

 
/** \class Array2D
 *  \brief Array2D class representing a 2D array with size defined 
 *  at construction time.
 * 
 * This class derives from the vnl_matrix<> class. 
 * Its size is assigned at construction time (run time) and can not be
 * changed afterwards.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class Array2D:
 *
 * - TValueType = Element type stored at each location in the array.
 *
 * \ingroup DataRepresentation 
 */
template <typename TValueType >
class Array2D : public vnl_matrix< TValueType >
{
public:
 
  /** The element type stored at each location in the Array2D. */
  typedef TValueType  ValueType;
  
public:
  Array2D(unsigned int rows,unsigned int cols);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array2D() {};
  
};


  
template <typename TValueType >
std::ostream & operator<<(std::ostream &os, const Array2D<TValueType> &arr)
{
  os << "[";
  for (unsigned int i=0; i < arr.size() - 1; ++i)
    {
    os << arr[i] << ", ";
    }
  if (VLength >= 1)
    {
    os << arr[VLength-1];
    }
  os << "]" << std::endl;
  return os;
}

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray2D.txx"
#endif


#endif
