/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 */

template <class T>
class ITK_EXPORT ByteSwapper : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ByteSwapper       Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ByteSwapper,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Query the machine Endian-ness
   */
  static bool IsBigEndian ();
  static bool IsBE () { return IsBigEndian(); };
  static bool IsLittleEndian ();
  static bool IsLE () { return IsLittleEndian(); };
  
  /**
   * Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian.
   */
  static void SwapBE(T *p);
  
  /**
   * Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian.
   */
  static void SwapRangeBE(T *p, unsigned long num);
  
  /**
   * Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Big Endian.
   */
  static void SwapWriteRangeBE(T *p, int num, std::ostream *fp);
  
  /**
   * Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian.
   */
  static void SwapLE(T *p);

  /**
   * Generic swap method handles type T. The swapping is
   * done in-place. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian.
   */
  static void SwapRangeLE(T *p, unsigned long num);

  /**
   * Generic swap method handles type T. The data is
   * swapped and written (in binary) to the ostream
   * given. A total of num values of type T are written
   * and swapped. Either 2-byte or 4-byte swapping
   * can be handled. Single byte types are not swapped;
   * others raise an exception. The method is used to
   * swap to and from Little Endian.
   */
  static void SwapWriteRangeLE(T *p, int num, std::ostream *fp);
  
protected:
  ByteSwapper() {};
  ~ByteSwapper() {};
  ByteSwapper(const ByteSwapper&) {};
  void operator=(const ByteSwapper&) {};

  /**
   * Swap 2 bytes.
   */
  static void Swap2(void *p);

  /**
   * Swap a range of two-byte words. Num is the number of two-byte 
   * words to swap.
   */
  static void Swap2Range(void *p, unsigned long num);

  /**
   * Swap and write a range of two-byte words. Num is the number of two-byte 
   * words to swap and write.
   */
  static void SwapWrite2Range(void *p, unsigned long num, std::ostream *fp);

  /**
   * Swap four bytes.
   */
  static void Swap4(void *p);

  /**
   * Swap a range of four-byte words. Num is the number of four-byte words 
   * to swap.
   */
  static void Swap4Range(void *p, unsigned long num);

  /**
   * Swap and write a range of four-byte words. Num is the number of four-byte 
   * words to swap and write.
   */
  static void SwapWrite4Range(void *p, unsigned long num, std::ostream *fp);

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkByteSwapper.txx"
#endif

#endif
