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
 * \ingroup ITKCommon
 */
template< typename TValueType >
class Array2D:public vnl_matrix< TValueType >
{
public:

  /** The element type stored at each location in the Array2D. */
  typedef TValueType               ValueType;
  typedef Array2D                  Self;
  typedef vnl_matrix< TValueType > VnlMatrixType;

public:

  Array2D();
  Array2D(unsigned int rows, unsigned int cols);
  Array2D(const Self & array);
  Array2D(const VnlMatrixType & matrix);

  const Self & operator=(const Self & array);

  const Self & operator=(const VnlMatrixType & matrix);

  void Fill(TValueType const & v);

  /** Destructively set the size to that given.  Will lose data.  */
  void SetSize(unsigned int m, unsigned int n);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~Array2D();
};

template< typename TValueType >
std::ostream & operator<<(std::ostream & os, const Array2D< TValueType > & arr)
{
  const unsigned int numberOfRows    = arr.rows();
  const unsigned int numberOfColumns = arr.cols();

  for ( unsigned int r = 0; r < numberOfRows; ++r )
    {
    os << "[";
    if ( numberOfColumns >= 1 )
      {
      const unsigned int lastColumn = numberOfColumns - 1;
      for ( unsigned int c = 0; c < lastColumn; ++c )
        {
        os << arr(r, c) << ", ";
        }
      os << arr(r, lastColumn);
      }
    os << "]" << std::endl;
    }

  return os;
}
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray2D.hxx"
#endif

#endif
