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

  /* Return the type of this pixel */
  typedef typename T::ValueType ValueType; 

  /* Return value of abs() */
  typedef typename T AbsType; 

  /* Accumulation of addition and multiplication */
  typedef typename double AccumulateType; 

  /* Additive identity */
  static const T Zero;

  /* Multiplicative identity */
  static const T One;

  /* Minimum value */
  static const T Min;

  /* Maximum value */
  static const T Max;

};

template <>
class itkPixelTraits<bool> {
public:
  typedef bool ValueType;
  typedef unsigned char AbsType;
  typedef unsigned char AccumulateType;
  static const bool Zero;
  static const bool One;
  static const bool Min;
  static const bool Max;
};

template <>
class itkPixelTraits<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char AbsType;
  typedef unsigned short AccumulateType;
  static const unsigned char Zero;
  static const unsigned char One;
  static const unsigned char Min;
  static const unsigned char Max;
};

template <>
class itkPixelTraits<signed char> {
public:
  typedef signed char ValueType;
  typedef unsigned char AbsType;
  typedef signed short AccumulateType;
  static const signed char Zero;
  static const signed char One;
  static const signed char Min;
  static const signed char Max;
};

template <>
class itkPixelTraits<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned short Zero;
  static const unsigned short One;
  static const unsigned short Min;
  static const unsigned short Max;
};

template <>
class itkPixelTraits<signed short> {
public:
  typedef signed short ValueType;
  typedef unsigned short AbsType;
  typedef signed int AccumulateType;
  static const signed short Zero;
  static const signed short One;
  static const signed short Min;
  static const signed short Max;
};

template <>
class itkPixelTraits<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned int Zero;
  static const unsigned int One;
  static const unsigned int Min;
  static const unsigned int Max;
};

template <>
class itkPixelTraits<signed int> {
public:
  typedef signed int ValueType;
  typedef unsigned int AbsType;
  typedef signed long AccumulateType;
  static const signed int Zero;
  static const signed int One;
  static const signed int Min;
  static const signed int Max;
};

template <>
class itkPixelTraits<signed long> {
public:
  typedef signed long ValueType;
  typedef unsigned long AbsType;
  typedef signed long AccumulateType;
  static const signed long Zero;
  static const signed long One;
  static const signed long Min;
  static const signed long Max;
};

template <>
class itkPixelTraits<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long AbsType;
  typedef unsigned long AccumulateType;
  static const unsigned long Zero;
  static const unsigned long One;
  static const unsigned long Min;
  static const unsigned long Max;
};

template <>
class itkPixelTraits<float> {
public:
  typedef float ValueType;
  typedef float AbsType;
  typedef double AccumulateType;
  static const float Zero;
  static const float One;
  static const float Min;
  static const float Max;
};

template <>
class itkPixelTraits<double> {
public:
  typedef double ValueType;
  typedef double AbsType;
  typedef double AccumulateType;
  static const double Zero;
  static const double One;
  static const double Min;
  static const double Max;
};

#endif // __itkPixelTraits_h
