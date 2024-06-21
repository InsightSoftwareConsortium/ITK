/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkArray2D_h
#define itkArray2D_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class Array2D
 *  \brief Array2D class representing a 2D array.
 *
 * This class derives from the vnl_matrix<> class.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class Array2D:
 *
 * - TValue = Element type stored at each location in the array.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <typename TValue>
class ITK_TEMPLATE_EXPORT Array2D : public vnl_matrix<TValue>
{
public:
  /** The element type stored at each location in the Array2D. */
  using ValueType = TValue;
  using Self = Array2D;
  using VnlMatrixType = vnl_matrix<TValue>;

  /** Default-constructor. Creates a matrix of size zero (0 rows and 0 columns). */
  Array2D() = default;

  /** Constructs a matrix of the specified number of rows and columns. */
  Array2D(unsigned int numberOfRows, unsigned int numberOfCols);

  /** Constructs a matrix of the specified number of rows and columns, with each element having the specified initial
   * value. */
  Array2D(unsigned int numberOfRows, unsigned int numberOfCols, const TValue & initialValue);

  /** Copy-constructor. */
  Array2D(const Self &) = default;

  /** Move-constructor.
   * \note This move-constructor is `noexcept`, even while the move-constructor of its base class (`vnl_matrix`) is not
   * `noexcept`, because unlike `vnl_matrix`, `Array2D` always manages its own memory. */
  Array2D(Self && array) noexcept
    : vnl_matrix<TValue>(std::move(array))
  {
    // Note: GCC <= 9.5 does not yet support "defaulting" (`= default`) this `noexcept` move-constructor.
  }

  /** Converting constructor. Implicitly converts the specified matrix to an Array2D. */
  Array2D(const VnlMatrixType & matrix);

  /** Copy-assignment operator. */
  Self &
  operator=(const Self &) = default;

  /** Move-assignment operator.
   * \note This move-assignment operator is `noexcept`, even while the move-assignment operator of its base class
   * (`vnl_matrix`) is not `noexcept`, because unlike `vnl_matrix`, `Array2D` always manages its own memory. */
  Self &
  operator=(Self && array) noexcept
  {
    // Note: GCC <= 9.5 does not yet support "defaulting" (`= default`) this `noexcept` move-assignment operator.
    this->VnlMatrixType::operator=(std::move(array));
    return *this;
  }

  /** Assigns the specified matrix to an Array2D. */
  Self &
  operator=(const VnlMatrixType & matrix);

  void
  Fill(TValue const & v)
  {
    this->fill(v);
  }

  /** Get one element */
  const TValue &
  GetElement(SizeValueType row, SizeValueType col) const
  {
    return this->operator()(static_cast<unsigned int>(row), static_cast<unsigned int>(col));
  }

  /** Set one element */
  void
  SetElement(SizeValueType row, SizeValueType col, const TValue & value)
  {
    this->operator()(static_cast<unsigned int>(row), static_cast<unsigned int>(col)) = value;
  }

  /** Destructively set the size to that given.  Will lose data.  */
  void
  SetSize(unsigned int m, unsigned int n);

  /** Destructor. */
  ~Array2D() override = default;
};

template <typename TValue>
std::ostream &
operator<<(std::ostream & os, const Array2D<TValue> & arr)
{
  const unsigned int numberOfRows = arr.rows();
  const unsigned int numberOfColumns = arr.cols();

  for (unsigned int r = 0; r < numberOfRows; ++r)
  {
    os << '[';
    if (numberOfColumns >= 1)
    {
      const unsigned int lastColumn = numberOfColumns - 1;
      for (unsigned int c = 0; c < lastColumn; ++c)
      {
        os << arr(r, c) << ", ";
      }
      os << arr(r, lastColumn);
    }
    os << ']' << std::endl;
  }

  return os;
}

// declaration of specialization
template <>
ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & os, const Array2D<float> & arr);
template <>
ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & os, const Array2D<double> & arr);

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkArray2D.hxx"
#endif

#endif
