/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxConversions.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxConversions_h
#define _cxxConversions_h

// Include all the type representations.
#include "cxxTypes.h"

namespace _cxx_
{

/**
 * A set of utilities for conversion tests.
 */
class _cxx_EXPORT Conversions
{
public:
  static bool IsValidQualificationConversion(const PointerType* from,
                                             const PointerType* to);
  static bool CanConvert(const CvQualifiedType& from,
                         const CvQualifiedType& to);
private:
  static bool ReferenceBinding(const CvQualifiedType& from,
                               const CvQualifiedType& to);
};

} // namespace _cxx_

#endif
