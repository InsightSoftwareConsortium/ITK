/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPixelTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkNumericTraits is used to determine characteristics of particular
 * pixel types. Traits include things like minimum and maximum value;
 * accumulation type; etc.
 */

#ifndef __itkPixelTraits_h
#define __itkPixelTraits_h

template <class T>
class itkPixelTraits {
public:

  /* Additive identity */
  static const T Zero;

  /* Multiplicative identity */
  static const T One;

  /* Return value of abs() */
  typedef T AbsType; 

  /* Accumulation of addition and multiplication */
  typedef double AccumulateType; 

  /* Minimum value */
  static const T Min;

  /* Maximum value */
  static const T Max;

  /* A character string describing the type. */
  static const char * Name;

};

template <>
class itkPixelTraits<bool> {
public:
  static const bool Zero;
  static const bool One;
  typedef unsigned char AbsType;
  typedef unsigned char AccumulateType;
  static const bool Min;
  static const bool Max;
  static const char* Name;
};

template <>
class itkPixelTraits<unsigned char> {
public:
  static const unsigned char Zero;
  static const unsigned char One;
  typedef unsigned char AbsType;
  typedef unsigned short AccumulateType;
  static const unsigned char Min;
  static const unsigned char Max;
  static const char* Name;
};

template <>
class itkPixelTraits<signed char> {
public:
  static const signed char Zero;
  static const signed char One;
  typedef unsigned char AbsType;
  typedef signed short AccumulateType;
  static const signed char Min;
  static const signed char Max;
  static const char* Name;
};

template <>
class itkPixelTraits<unsigned short> {
public:
  static const unsigned short Zero;
  static const unsigned short One;
  typedef unsigned short AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned short Min;
  static const unsigned short Max;
  static const char* Name;
};

template <>
class itkPixelTraits<signed short> {
public:
  static const signed short Zero;
  static const signed short One;
  typedef unsigned short AbsType;
  typedef signed int AccumulateType;
  static const signed short Min;
  static const signed short Max;
  static const char* Name;
};

template <>
class itkPixelTraits<unsigned int> {
public:
  static const unsigned int Zero;
  static const unsigned int One;
  typedef unsigned int AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned int Min;
  static const unsigned int Max;
  static const char* Name;
};

template <>
class itkPixelTraits<signed int> {
public:
  static const signed int Zero;
  static const signed int One;
  typedef unsigned int AbsType;
  typedef signed long AccumulateType;
  static const signed int Min;
  static const signed int Max;
  static const char* Name;
};

template <>
class itkPixelTraits<signed long> {
public:
  static const signed long Zero;
  static const signed long One;
  typedef unsigned long AbsType;
  typedef signed long AccumulateType;
  static const signed long Min;
  static const signed long Max;
  static const char* Name;
};

template <>
class itkPixelTraits<unsigned long> {
public:
  static const unsigned long Zero;
  static const unsigned long One;
  typedef unsigned long AbsType;
  typedef unsigned long AccumulateType;
  static const unsigned long Min;
  static const unsigned long Max;
  static const char* Name;
};

template <>
class itkPixelTraits<float> {
public:
  static const float Zero;
  static const float One;
  typedef float AbsType;
  typedef double AccumulateType;
  static const float Min;
  static const float Max;
  static const char* Name;
};

template <>
class itkPixelTraits<double> {
public:
  static const double Zero;
  static const double One;
  typedef double AbsType;
  typedef double AccumulateType;
  static const double Min;
  static const double Max;
  static const char* Name;
};

#endif // __itkPixelTraits_h
