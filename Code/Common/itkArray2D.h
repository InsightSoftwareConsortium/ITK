/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  typedef TValueType              ValueType;
  typedef Array2D                 Self;
  typedef vnl_matrix<TValueType>  VnlMatrixType;
  
public:

  Array2D();
  Array2D(unsigned int rows,unsigned int cols);
  Array2D( const Self & array );
  Array2D( const VnlMatrixType & matrix );

  const Self & operator=( const Self & array );
  const Self & operator=( const VnlMatrixType & matrix );

  void Fill (TValueType const& v) { fill(v); }
  
  /** Destructively set the size to that given.  Will lose data.  */
  void SetSize(unsigned int m, unsigned int n);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array2D() {};
  
};


  
template <typename TValueType >
std::ostream & operator<<(std::ostream &os, const Array2D<TValueType> &arr)
{
  const unsigned int numberOfColumns = arr.cols();
  const unsigned int numberOfRows    = arr.rows();
  const signed int lastColumn = (signed int) numberOfColumns - 1;

  for (unsigned int r=0; r < numberOfRows; ++r)
    {
    os << "[";
    for ( signed int c=0; c < lastColumn; ++c)
      {
      os << arr(r,c) << ", ";
      }
    if (numberOfColumns >= 1)
      {
      os << arr(r,lastColumn);
      }
    os << "]" << std::endl;
    }

  return os;
}

} // namespace itk


// Define instantiation macro for this template.
#define ITK_TEMPLATE_Array2D(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT Array2D< ITK_TEMPLATE_1 x >)) \
  _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                     const Array2D< ITK_TEMPLATE_1 x >&))) \
  namespace Templates { typedef Array2D< ITK_TEMPLATE_1 x > Array2D##y; } \
  }


#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkArray2D+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkArray2D.txx"
#endif

#endif
