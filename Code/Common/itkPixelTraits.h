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
 * PixelTraits is used to determine characteristics of particular pixel
 * types. Pixels are can be user-defined types (not just the native types),
 * and may consist of a scalar portion and a vector portion. Depending on how
 * you want to treat the pixel (as either a scalar or vector), you can use
 * the ScalarTraits and VectorTraits to determine characteristics of
 * the pixel.  
 */

#ifndef __itkPixelTraits_h
#define __itkPixelTraits_h

#include "itkNumericTraits.h"

namespace itk
{

template <class T>
class ScalarTraits {
public:
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType ValueType;
  
  /* 
   * Support GetScalar method 
   */
  typedef typename T::ValueType ScalarType;
  
  /* 
   * Support the GetScalar / SetScalar methods. These are the
   * default implemnentations if the template is not specialized.
   */
  static NumericTraits<typename T::ValueType>::ValueType& GetScalar(T& v) {return v.GetScalar();}
  //static ScalarType& GetScalar(T& v) {return v.GetScalar();}
  static void SetScalar(T& v, ScalarType const&d) {v.SetScalar(d);}
};

template <class T>
class VectorTraits {
public:
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType ValueType;
  
  /* 
   * Support GetVector method 
   */
  typedef typename T::ValueType VectorType;
  
  /* 
   * Support the GetVector / SetVector methods. These are the
   * default implemnentations if the template is not specialized.
   */
  static VectorType& GetVector(T& v) {return v.GetVector();}
  static void SetVector(T& v, VectorType const&d) {v.SetVector(d);}
};


/**
 * The following are specializations of the ScalarTraits for the native
 * types.
 */
template <>
class ScalarTraits<bool> {
public:
  typedef bool ValueType;
  typedef bool ScalarType;
  static ScalarType& GetScalar(bool& v) {return v;}
  static void SetScalar(bool& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char ScalarType;
  static ScalarType& GetScalar(unsigned char& v) {return v;}
  static void SetScalar(unsigned char& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<signed char> {
public:
  typedef signed char ValueType;
  typedef signed char ScalarType;
  static ScalarType& GetScalar(signed char& v) {return v;}
  static void SetScalar(signed char& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short ScalarType;
  static ScalarType& GetScalar(unsigned short& v) {return v;}
  static void SetScalar(unsigned short& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<signed short> {
public:
  typedef signed short ValueType;
  typedef signed short ScalarType;
  static ScalarType& GetScalar(signed short& v) {return v;}
  static void SetScalar(signed short& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int ScalarType;
  static ScalarType& GetScalar(unsigned int& v) {return v;}
  static void SetScalar(unsigned int& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<signed int> {
public:
  typedef signed int ValueType;
  typedef signed int ScalarType;
  static ScalarType& GetScalar(signed int& v) {return v;}
  static void SetScalar(signed int& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<signed long> {
public:
  typedef signed long ValueType;
  typedef signed long ScalarType;
  static ScalarType& GetScalar(signed long& v) {return v;}
  static void SetScalar(signed long& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long ScalarType;
  static ScalarType& GetScalar(unsigned long& v) {return v;}
  static void SetScalar(unsigned long& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<float> {
public:
  typedef float ValueType;
  typedef float ScalarType;
  static ScalarType& GetScalar(float& v) {return v;}
  static void SetScalar(float& data, ScalarType const& v) {data = v;}
};

template <>
class ScalarTraits<double> {
public:
  typedef double ValueType;
  typedef double ScalarType;
  static ScalarType& GetScalar(double& v) {return v;}
  static void SetScalar(double& data, ScalarType const& v) {data = v;}
};


/**
 * The following are specializations of the VectorTraits for the native
 * types.
 */
template <>
class VectorTraits<bool> {
public:
  typedef bool ValueType;
  typedef bool VectorType;
  static VectorType& GetVector(bool& v) {return v;}
  static void SetVector(bool& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char VectorType;
  static VectorType& GetVector(unsigned char& v) {return v;}
  static void SetVector(unsigned char& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<signed char> {
public:
  typedef signed char ValueType;
  typedef signed char VectorType;
  static VectorType& GetVector(signed char& v) {return v;}
  static void SetVector(signed char& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short VectorType;
  static VectorType& GetVector(unsigned short& v) {return v;}
  static void SetVector(unsigned short& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<signed short> {
public:
  typedef signed short ValueType;
  typedef signed short VectorType;
  static VectorType& GetVector(signed short& v) {return v;}
  static void SetVector(signed short& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int VectorType;
  static VectorType& GetVector(unsigned int& v) {return v;}
  static void SetVector(unsigned int& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<signed int> {
public:
  typedef signed int ValueType;
  typedef signed int VectorType;
  static VectorType& GetVector(signed int& v) {return v;}
  static void SetVector(signed int& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<signed long> {
public:
  typedef signed long ValueType;
  typedef signed long VectorType;
  static VectorType& GetVector(signed long& v) {return v;}
  static void SetVector(signed long& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long VectorType;
  static VectorType& GetVector(unsigned long& v) {return v;}
  static void SetVector(unsigned long& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<float> {
public:
  typedef float ValueType;
  typedef float VectorType;
  static VectorType& GetVector(float& v) {return v;}
  static void SetVector(float& data, VectorType const& v) {data = v;}
};

template <>
class VectorTraits<double> {
public:
  typedef double ValueType;
  typedef double VectorType;
  static VectorType& GetVector(double& v) {return v;}
  static void SetVector(double& data, VectorType const& v) {data = v;}
};

} // namespace itk

#endif // __itkPixelTraits_h
