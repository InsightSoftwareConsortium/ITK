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
#ifndef __itkPixelTraits_h
#define __itkPixelTraits_h

#include "itkNumericTraits.h"

namespace itk
{

/** \class ScalarTraits
 * \brief Determine characterisitics of a scalar pixel type.
 *
 * PixelTraits is used to determine characteristics of particular pixel
 * types. Pixels are can be user-defined types (not just the native types),
 * and may consist of a scalar portion and a vector portion. Depending on how
 * you want to treat the pixel (as either a scalar or vector), you can use
 * the ScalarTraits and VectorTraits to determine characteristics of
 * the pixel.  
 */

template <class T>
class ScalarTraits {
public:
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType ValueType;
  
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType ScalarValueType;
  
  /* 
   * Support the GetScalar / SetScalar methods. These are the
   * default implemnentations if the template is not specialized.
   */
  static NumericTraits<typename T::ScalarValueType>::ValueType& GetScalar(T& v) {return v.GetScalar();}
  //static ScalarValueType& GetScalar(T& v) {return v.GetScalar();}
  static void SetScalar(T& v, ScalarValueType const&d) {v.SetScalar(d);}
};

/** \class VectorTraits
 * \brief Determine characterisitics of a vector pixel type.
 *
 * PixelTraits is used to determine characteristics of particular pixel
 * types. Pixels are can be user-defined types (not just the native types),
 * and may consist of a scalar portion and a vector portion. Depending on how
 * you want to treat the pixel (as either a scalar or vector), you can use
 * the ScalarTraits and VectorTraits to determine characteristics of
 * the pixel.  
 */
template <class T>
class VectorTraits {
public:
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType ValueType;
  
  /* 
   * Reflect the value type from the underlying (usually native type)
   * value type.
   */
  typedef typename T::ValueType VectorValueType;
  
  /* 
   * Support the GetVector / SetVector methods. These are the
   * default implemnentations if the template is not specialized.
   */
  static VectorValueType& GetVector(T& v) {return v.GetVector();}
  static void SetVector(T& v, VectorValueType const&d) {v.SetVector(d);}
};


/**
 * The following are specializations of the ScalarTraits for the native
 * types.
 */

/** \class ScalarTraits<bool>
 * \brief Define (pixel) scalar traits for type bool.
 */
template <>
class ScalarTraits<bool> {
public:
  typedef bool ValueType;
  typedef bool ScalarValueType;
  static ScalarValueType& GetScalar(bool& v) {return v;}
  static void SetScalar(bool& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<unsigned char>
 * \brief Define (pixel) scalar traits for type unsigned char.
 */
template <>
class ScalarTraits<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char ScalarValueType;
  static ScalarValueType& GetScalar(unsigned char& v) {return v;}
  static void SetScalar(unsigned char& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<signed char>
 * \brief Define (pixel) scalar traits for type signed char.
 */
template <>
class ScalarTraits<signed char> {
public:
  typedef signed char ValueType;
  typedef signed char ScalarValueType;
  static ScalarValueType& GetScalar(signed char& v) {return v;}
  static void SetScalar(signed char& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<unsigned short>
 * \brief Define (pixel) scalar traits for type unsigned short.
 */
template <>
class ScalarTraits<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short ScalarValueType;
  static ScalarValueType& GetScalar(unsigned short& v) {return v;}
  static void SetScalar(unsigned short& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<signed short>
 * \brief Define (pixel) scalar traits for type signed short.
 */
template <>
class ScalarTraits<signed short> {
public:
  typedef signed short ValueType;
  typedef signed short ScalarValueType;
  static ScalarValueType& GetScalar(signed short& v) {return v;}
  static void SetScalar(signed short& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<unsigned int>
 * \brief Define (pixel) scalar traits for type unsigned int.
 */
template <>
class ScalarTraits<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int ScalarValueType;
  static ScalarValueType& GetScalar(unsigned int& v) {return v;}
  static void SetScalar(unsigned int& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<signed int>
 * \brief Define (pixel) scalar traits for type signed int.
 */
template <>
class ScalarTraits<signed int> {
public:
  typedef signed int ValueType;
  typedef signed int ScalarValueType;
  static ScalarValueType& GetScalar(signed int& v) {return v;}
  static void SetScalar(signed int& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<signed long>
 * \brief Define (pixel) scalar traits for type signed long.
 */
template <>
class ScalarTraits<signed long> {
public:
  typedef signed long ValueType;
  typedef signed long ScalarValueType;
  static ScalarValueType& GetScalar(signed long& v) {return v;}
  static void SetScalar(signed long& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<unsigned long>
 * \brief Define (pixel) scalar traits for type unsigned long.
 */
template <>
class ScalarTraits<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long ScalarValueType;
  static ScalarValueType& GetScalar(unsigned long& v) {return v;}
  static void SetScalar(unsigned long& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<float>
 * \brief Define (pixel) scalar traits fortype <float.
 */
template <>
class ScalarTraits<float> {
public:
  typedef float ValueType;
  typedef float ScalarValueType;
  static ScalarValueType& GetScalar(float& v) {return v;}
  static void SetScalar(float& data, ScalarValueType const& v) {data = v;}
};

/** \class ScalarTraits<double>
 * \brief Define (pixel) scalar traits fortype <double.
 */
template <>
class ScalarTraits<double> {
public:
  typedef double ValueType;
  typedef double ScalarValueType;
  static ScalarValueType& GetScalar(double& v) {return v;}
  static void SetScalar(double& data, ScalarValueType const& v) {data = v;}
};

/**
 * The following are specializations of the VectorTraits for the native
 * types.
 */

/** \class VectorTraits<bool>
 * \brief Define (pixel) vector traits for type bool.
 */
template <>
class VectorTraits<bool> {
public:
  typedef bool ValueType;
  typedef bool VectorValueType;
  static VectorValueType& GetVector(bool& v) {return v;}
  static void SetVector(bool& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<unsigned char>
 * \brief Define (pixel) vector traits for type unsigned char.
 */
template <>
class VectorTraits<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char VectorValueType;
  static VectorValueType& GetVector(unsigned char& v) {return v;}
  static void SetVector(unsigned char& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<signed char>
 * \brief Define (pixel) vector traits for type signed char.
 */
template <>
class VectorTraits<signed char> {
public:
  typedef signed char ValueType;
  typedef signed char VectorValueType;
  static VectorValueType& GetVector(signed char& v) {return v;}
  static void SetVector(signed char& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<unsigned short>
 * \brief Define (pixel) vector traits for type unsigned short.
 */
template <>
class VectorTraits<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short VectorValueType;
  static VectorValueType& GetVector(unsigned short& v) {return v;}
  static void SetVector(unsigned short& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<signed short>
 * \brief Define (pixel) vector traits for type signed short.
 */
template <>
class VectorTraits<signed short> {
public:
  typedef signed short ValueType;
  typedef signed short VectorValueType;
  static VectorValueType& GetVector(signed short& v) {return v;}
  static void SetVector(signed short& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<unsigned int>
 * \brief Define (pixel) vector traits for type unsigned int.
 */
template <>
class VectorTraits<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int VectorValueType;
  static VectorValueType& GetVector(unsigned int& v) {return v;}
  static void SetVector(unsigned int& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<signed int>
 * \brief Define (pixel) vector traits for type signed int.
 */
template <>
class VectorTraits<signed int> {
public:
  typedef signed int ValueType;
  typedef signed int VectorValueType;
  static VectorValueType& GetVector(signed int& v) {return v;}
  static void SetVector(signed int& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<signed long>
 * \brief Define (pixel) vector traits for type signed long.
 */
template <>
class VectorTraits<signed long> {
public:
  typedef signed long ValueType;
  typedef signed long VectorValueType;
  static VectorValueType& GetVector(signed long& v) {return v;}
  static void SetVector(signed long& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<unsigned long>
 * \brief Define (pixel) vector traits for type unsigned long.
 */
template <>
class VectorTraits<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long VectorValueType;
  static VectorValueType& GetVector(unsigned long& v) {return v;}
  static void SetVector(unsigned long& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<float>
 * \brief Define (pixel) vector traits for type float.
 */
template <>
class VectorTraits<float> {
public:
  typedef float ValueType;
  typedef float VectorValueType;
  static VectorValueType& GetVector(float& v) {return v;}
  static void SetVector(float& data, VectorValueType const& v) {data = v;}
};

/** \class VectorTraits<double>
 * \brief Define (pixel) vector traits for type double.
 */
template <>
class VectorTraits<double> {
public:
  typedef double ValueType;
  typedef double VectorValueType;
  static VectorValueType& GetVector(double& v) {return v;}
  static void SetVector(double& data, VectorValueType const& v) {data = v;}
};

} // end namespace itk

#endif // __itkPixelTraits_h
