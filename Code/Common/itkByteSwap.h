/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwap.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkByteSwap_h
#define __itkByteSwap_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class ByteSwap
 * \brief Perform machine dependent byte swapping.
 *
 * ByteSwap is used by I/O classes to perform machine dependent byte
 * swapping. Byte swapping is often used when reading or writing binary 
 * files. Files can either be Big Endian (BE) or Little Endian (LE).
 */

template <class T>
class ITK_EXPORT ByteSwap : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ByteSwap       Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ByteSwap,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
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
  ByteSwap() {};
  ~ByteSwap() {};
  ByteSwap(const ByteSwap&) {};
  void operator=(const ByteSwap&) {};

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
#include "itkByteSwap.txx"
#endif

#endif
