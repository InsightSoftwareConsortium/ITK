/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIdentifierTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkIdentifierTraits_h
#define __itkIdentifierTraits_h

namespace itk
{

/**
 * IdentifierTraits
 *
 * Type information for identifier types used to index various itk containers.
 */

template <typename T>
class IdentifierTraits
{
public:
  /**
   * Is the identifier of an integral type?  If it is, it can be used to
   * index random-access containers.
   */
  static const bool IsIntegralType;
};


/**
 * Create specialized trait sets for each pre-defined identifier type.
 */
template <>
class IdentifierTraits<char>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<unsigned char>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<short>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<unsigned short>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<int>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<unsigned int>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<long>
{
public:
  static const bool IsIntegralType;
};

template <>
class IdentifierTraits<unsigned long>
{
public:
  static const bool IsIntegralType;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIdentifierTraits.txx"
#endif

#endif
