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

template< typename T >
class ITK_TEMPLATE_EXPORT ByteSwapper:public Object
{
public:
  /** Standard class typedefs. */
  typedef ByteSwapper                Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Work around MSVC bug (including ByteSwapper.h in a templated class). */
  typedef std::ostream OStreamType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ByteSwapper, Object);

  /** Query the machine Endian-ness. */
  static bool SystemIsBigEndian();

  static bool SystemIsBE() { return SystemIsBigEndian(); }
  static bool SystemIsLittleEndian();

  static bool SystemIsLE() { return SystemIsLittleEndian(); }

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapFromSystemToBigEndian(T *p);

  /** Type for representing large buffers, including those in 64bits
   * architectures */
  typedef SizeValueType BufferSizeType;

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapRangeFromSystemToBigEndian(T *p, BufferSizeType num);

  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapWriteRangeFromSystemToBigEndian(T *p, int num,
                                                  OStreamType *fp);

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapFromSystemToLittleEndian(T *p);

  /** Generic swap method handles type T. The swapping is
   * done in-place. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapRangeFromSystemToLittleEndian(T *p, BufferSizeType num);

  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. 2, 4 and 8 byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapWriteRangeFromSystemToLittleEndian(T *p, int num,
                                                     OStreamType *fp);

protected:
  ByteSwapper() {}
  ~ByteSwapper() ITK_OVERRIDE {}

  /** Swap 2 bytes. */
  static void Swap2(void *p);

  /** Swap a range of two-byte words. Num is the number of two-byte
   * words to swap. */
  static void Swap2Range(void *p, BufferSizeType num);

  /** Swap and write a range of two-byte words. Num is the number of two-byte
   * words to swap and write. */
  static void SwapWrite2Range(void *p, BufferSizeType num, OStreamType *fp);

  /** Swap four bytes. */
  static void Swap4(void *p);

  /** Swap a range of four-byte words. Num is the number of four-byte words
   * to swap. */
  static void Swap4Range(void *p, BufferSizeType num);

  /** Swap and write a range of four-byte words. Num is the number of four-byte
   * words to swap and write. */
  static void SwapWrite4Range(void *p, BufferSizeType num, OStreamType *fp);

  /** Swap 8 bytes. */
  static void Swap8(void *p);

  /** Swap a range of 8-byte words. Num is the number of four-byte words
   * to swap. */
  static void Swap8Range(void *p, BufferSizeType num);

  /** Swap and write a range of 8-byte words. Num is the number of four-byte
   * words to swap and write. */
  static void SwapWrite8Range(void *p, BufferSizeType num, OStreamType *fp);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ByteSwapper);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkByteSwapper.hxx"
#endif

#endif
