/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkByteSwapper_h
#define __itkByteSwapper_h

#include "itkObject.h"
#include "itkObjectFactory.h"

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
 */

template <class T>
class ITK_EXPORT ByteSwapper : public Object
{
public:
  /** Standard class typedefs. */
  typedef ByteSwapper       Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Work around MSVC bug (including ByteSwapper.h in a templated class). */
  typedef std::ostream OStreamType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ByteSwapper,Object);

  /** Query the machine Endian-ness. */
  static bool SystemIsBigEndian ();
  static bool SystemIsBE () { return SystemIsBigEndian(); }
  static bool SystemIsLittleEndian ();
  static bool SystemIsLE () { return SystemIsLittleEndian(); }
    
  /** Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapFromSystemToBigEndian(T *p);
  
  /** Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapRangeFromSystemToBigEndian(T *p, unsigned long num);
  
  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian. */
  static void SwapWriteRangeFromSystemToBigEndian(T *p, int num, OStreamType *fp);
  
  /** Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapFromSystemToLittleEndian(T *p);

  /** Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapRangeFromSystemToLittleEndian(T *p, unsigned long num);

  /** Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian. */
  static void SwapWriteRangeFromSystemToLittleEndian(T *p, int num, OStreamType *fp);
  
protected:
  ByteSwapper() {}
  ~ByteSwapper() {}

  /** Swap 2 bytes. */
  static void Swap2(void *p);

  /** Swap a range of two-byte words. Num is the number of two-byte 
   * words to swap. */
  static void Swap2Range(void *p, unsigned long num);

  /** Swap and write a range of two-byte words. Num is the number of two-byte 
   * words to swap and write. */
  static void SwapWrite2Range(void *p, unsigned long num, OStreamType *fp);

  /** Swap four bytes. */
  static void Swap4(void *p);

  /** Swap a range of four-byte words. Num is the number of four-byte words 
   * to swap. */
  static void Swap4Range(void *p, unsigned long num);

  /** Swap and write a range of four-byte words. Num is the number of four-byte 
   * words to swap and write. */
  static void SwapWrite4Range(void *p, unsigned long num, OStreamType *fp);

  /** Swap 8 bytes. */
  static void Swap8(void *p);

  /** Swap a range of four-byte words. Num is the number of four-byte words 
   * to swap. */
  static void Swap8Range(void *p, unsigned long num);

  /** Swap and write a range of 8-byte words. Num is the number of four-byte 
   * words to swap and write. */
  static void SwapWrite8Range(void *p, unsigned long num, OStreamType *fp);

private:
  ByteSwapper(const ByteSwapper&); //purposely not implemented
  void operator=(const ByteSwapper&); //purposely not implemented
  
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkByteSwapper.txx"
#endif

#endif
