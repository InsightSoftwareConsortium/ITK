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
   * Swap 2 byte word to be LE.
   */
  static void Swap2LE(void *p);

  /**
   * Swap four byte word to be LE.
   */
  static void Swap4LE(void *p);

  /**
   * Swap bunch of bytes to be LE. Num is the number of four byte words 
   * to swap.
   */
  static void Swap4LERange(void *p, int num);

  /**
   * Swap four byte word to be BE.
   */
  static void Swap4BE(void *p);
  
  /**
   * Swap bunch of bytes to be BE. Num is the number of four byte words 
   * to swap.
   */
  static void Swap4BERange(void *p, int num);

  /**
   * Swap bunch of bytes to BE. Num is the number of four byte words to swap.
   * The results are written out to prevent having to keep the swapped
   * copy in memory.
   */
  static void SwapWrite4BERange(void *p, int num, std::ostream *fp);

  /**
   * Swap 2 byte word to BE.
   */
  static void Swap2BE(void *p);

  /**
   * Swap bunch of bytes to BE. Num is the number of two byte words to swap.
   */
  static void Swap2BERange(void *p, int num);

  /**
   * Swap bunch of bytes to LE. Num is the number of two byte words to swap.
   */
  static void Swap2LERange(void *p, int num);

  /**
   * Swap bunch of bytes to BE. Num is the number of two byte words to swap.
   * The results are written out to prevent having to keep the swapped
   * copy in memory.
   */
  static void SwapWrite2BERange(void *p, int num, std::ostream *fp);

  /**
   * Swaps the bytes of a buffer.  Uses an arbitrary word size, but
   * assumes the word size is divisible by two.
   */
  static void SwapVoidRange(void *buffer, int numWords, int wordSize);

protected:
  ByteSwap() {};
  ~ByteSwap() {};
  ByteSwap(const ByteSwap&) {};
  void operator=(const ByteSwap&) {};

};

} // end namespace itk
  
#endif
