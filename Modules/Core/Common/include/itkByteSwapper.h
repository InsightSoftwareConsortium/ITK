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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkByteSwapper_h
#define itkByteSwapper_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class ByteSwapper
 * \brief Perform machine dependent byte swapping.
 *
 * ByteSwapper is used by I/O classes to perform machine dependent byte
 * swapping. Byte swapping is often used when reading or writing binary
 * files. Files can either be Big Endian (BE) or Little Endian (LE).
 *
 * \ingroup IOFilters
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */

template <typename T>
class ITK_TEMPLATE_EXPORT ByteSwapper : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ByteSwapper);

  /** Standard class type aliases. */
  using Self = ByteSwapper;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

#ifndef ITK_FUTURE_LEGACY_REMOVE
  /** Work around old MSVC bug (including ByteSwapper.h in a templated class). */
  using OStreamType
    [[deprecated("ByteSwapper::OStreamType is deprecated from ITK 6. Just use `std::ostream` instead!")]] =
      std::ostream;
#endif

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ByteSwapper);

  /** Query the machine Endian-ness.
  \note C++20 also allows querying the endianness, by using its enum class `std::endian`. */
  static constexpr bool
  SystemIsBigEndian()
  {
    return m_SystemIsBigEndian;
  }

  static constexpr bool
  SystemIsBE()
  {
    return SystemIsBigEndian();
  }
  static constexpr bool
  SystemIsLittleEndian()
  {
    return !m_SystemIsBigEndian;
  }

  static constexpr bool
  SystemIsLE()
  {
    return SystemIsLittleEndian();
  }

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void
  SwapFromSystemToBigEndian(T * p);

  /** Type for representing large buffers, including those in 64bits
   * architectures */
  using BufferSizeType = SizeValueType;

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void
  SwapRangeFromSystemToBigEndian(T * p, BufferSizeType num);

  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void
  SwapWriteRangeFromSystemToBigEndian(const T * p, int num, std::ostream * fp);

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void
  SwapFromSystemToLittleEndian(T * p);

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void
  SwapRangeFromSystemToLittleEndian(T * p, BufferSizeType num);

  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void
  SwapWriteRangeFromSystemToLittleEndian(const T * p, int num, std::ostream * fp);

protected:
  ByteSwapper() = default;
  ~ByteSwapper() override = default;

private:
  /** Swaps the bytes of the specified argument in-place. Assumes that its number of bytes is either 2, 4, or 8.
   * Otherwise, it throws an exception. */
  static void
  SwapBytes(T &);

  /** Swaps and writes the specified elements to the specified output stream. */
  static void
  SwapWriteRange(const T * buffer, SizeValueType numberOfElements, std::ostream & outputStream);

  static constexpr bool m_SystemIsBigEndian{
#ifdef CMAKE_WORDS_BIGENDIAN
    true
#else
    false
#endif
  };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkByteSwapper.hxx"
#endif

#endif
