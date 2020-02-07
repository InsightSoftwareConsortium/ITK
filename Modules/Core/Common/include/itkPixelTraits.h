/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPixelTraits_h
#define itkPixelTraits_h

#include "itkMacro.h"

namespace itk
{
/** \class PixelTraits
 * \brief Traits for a pixel that define the dimension and component type.
 *
 * PixelTraits determines the dimension and the component type
 * of a pixel.  The default implementation is suitable for all subclasses
 * of itk::Array. This (will) include RGBPixel and RGBAPixel. Specialized
 * versions of PixelTraits are defined for the standard scalar types.
 * \ingroup ITKCommon
 */
template <typename TPixelType>
class PixelTraits
{
public:
  /** Dimension of the pixel (range). */
  static constexpr unsigned int Dimension = TPixelType::Length;

  /** Type of a single component of a pixel. */
  using ValueType = typename TPixelType::ValueType;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

/** \class PixelTraits<bool>
 * Specialization of PixelTraits for scalar images.
 * \ingroup ITKCommon
 */
template <>
class PixelTraits<bool>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = bool;
};

template <>
class PixelTraits<char>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = char;
};

template <>
class PixelTraits<signed char>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = char;
};

template <>
class PixelTraits<unsigned char>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = unsigned char;
};

template <>
class PixelTraits<short>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = short;
};

template <>
class PixelTraits<unsigned short>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = unsigned short;
};

template <>
class PixelTraits<int>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = int;
};

template <>
class PixelTraits<unsigned int>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = unsigned int;
};

template <>
class PixelTraits<long>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = long;
};

template <>
class PixelTraits<unsigned long>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = unsigned long;
};


template <>
class PixelTraits<long long>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = long long;
};

template <>
class PixelTraits<unsigned long long>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = unsigned long long;
};

template <>
class PixelTraits<float>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = float;
};

template <>
class PixelTraits<double>
{
public:
  static constexpr unsigned int Dimension = 1;
  using ValueType = double;
};

/// \endcond

/** \class JoinTraits
 * \brief Trait to determine what datatype is needed if the specified
 * pixel types are "joined" into a single vector.
 *
 * JoinTraits defines the value type needed to combine the specified
 * pixel types into a single vector.  The data type selected is the
 * smallest data type that can represent the dynamic range of both
 * input pixel types.  For example, if a char and unsigned short are
 * "joined", the resulting data type must be a vector of int.  In
 * some cases, like joining a unsigned int and a char, the join
 * value type is promoted all the way to a float.  This provides
 * consistent behavior on both 32 and 64 bit systems (on 64 bit
 * systems, we could have promoted to a long which is distinct from
 * an int but this is not the case for 32 bit systems, so we promote
 * to float). There are several combinations similar to this.  Most
 * of the JoinTraits are specializations of the base template.
 * \ingroup ITKCommon
 */
template <typename TValue1, typename TValue2>
class JoinTraits
{
public:
  using ValueType = TValue1;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

/** \class JoinTraits
 * Specializations for bool.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<bool, bool>
{
public:
  using ValueType = bool;
};

template <>
class JoinTraits<bool, char>
{
public:
  using ValueType = char;
};

template <>
class JoinTraits<bool, unsigned char>
{
public:
  using ValueType = unsigned char;
};

template <>
class JoinTraits<bool, short>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<bool, unsigned short>
{
public:
  using ValueType = unsigned short;
};

template <>
class JoinTraits<bool, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<bool, unsigned int>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<bool, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<bool, unsigned long>
{
public:
  using ValueType = unsigned long;
};


template <>
class JoinTraits<bool, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<bool, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<bool, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<bool, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<char>
 * Specializations for char.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<char, bool>
{
public:
  using ValueType = char;
};

template <>
class JoinTraits<char, char>
{
public:
  using ValueType = char;
};

template <>
class JoinTraits<char, unsigned char>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<char, short>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<char, unsigned short>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<char, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<char, unsigned int>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<char, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<char, unsigned long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<char, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<char, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<char, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<char, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<unsigned char>
 * Specializations for unsigned char.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<unsigned char, bool>
{
public:
  using ValueType = unsigned char;
};

template <>
class JoinTraits<unsigned char, char>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<unsigned char, unsigned char>
{
public:
  using ValueType = unsigned char;
};

template <>
class JoinTraits<unsigned char, short>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<unsigned char, unsigned short>
{
public:
  using ValueType = unsigned short;
};

template <>
class JoinTraits<unsigned char, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<unsigned char, unsigned int>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned char, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<unsigned char, unsigned long>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned char, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<unsigned char, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned char, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned char, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<short>
 * Specializations for short.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<short, bool>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<short, char>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<short, unsigned char>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<short, short>
{
public:
  using ValueType = short;
};

template <>
class JoinTraits<short, unsigned short>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<short, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<short, unsigned int>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<short, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<short, unsigned long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<short, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<short, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<short, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<short, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<unsigned short>
 * Specializations for unsigned short.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<unsigned short, bool>
{
public:
  using ValueType = unsigned short;
};

template <>
class JoinTraits<unsigned short, char>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<unsigned short, unsigned char>
{
public:
  using ValueType = unsigned short;
};

template <>
class JoinTraits<unsigned short, short>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<unsigned short, unsigned short>
{
public:
  using ValueType = unsigned short;
};

template <>
class JoinTraits<unsigned short, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<unsigned short, unsigned int>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned short, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<unsigned short, unsigned long>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned short, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<unsigned short, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned short, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned short, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<int>
 * Specializations for int.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<int, bool>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, char>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, unsigned char>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, short>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, unsigned short>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, int>
{
public:
  using ValueType = int;
};

template <>
class JoinTraits<int, unsigned int>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<int, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<int, unsigned long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<int, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<int, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<int, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<int, double>
{
public:
  using ValueType = double;
};

/**  \class PixelTraits<unsigned int>
 * Specializations for unsigned int.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<unsigned int, bool>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned int, char>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<unsigned int, unsigned char>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned int, short>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<unsigned int, unsigned short>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned int, int>
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  using ValueType = float;
};

template <>
class JoinTraits<unsigned int, unsigned int>
{
public:
  using ValueType = unsigned int;
};

template <>
class JoinTraits<unsigned int, long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned int, unsigned long>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned int, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<unsigned int, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned int, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned int, double>
{
public:
  using ValueType = double;
};

/** \class PixelTraits<long>
 * Specializations for long.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<long, bool>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, char>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, unsigned char>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, short>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, unsigned short>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, int>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, unsigned int>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<long, long>
{
public:
  using ValueType = long;
};

template <>
class JoinTraits<long, unsigned long>
{
public:
  using ValueType = float;
};


template <>
class JoinTraits<long, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<long, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<long, double>
{
public:
  using ValueType = double;
};

/** \class PixelTraits<unsigned long>
 * Specializations for unsigned long.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<unsigned long, bool>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned long, char>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned long, unsigned char>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned long, short>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned long, unsigned short>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned long, int>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned long, unsigned int>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned long, long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned long, unsigned long>
{
public:
  using ValueType = unsigned long;
};

template <>
class JoinTraits<unsigned long, long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<unsigned long, double>
{
public:
  using ValueType = double;
};


/** \class PixelTraits<long long>
 * Specializations for long long.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<long long, bool>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, char>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, unsigned char>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, short>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, unsigned short>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, int>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, unsigned int>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, unsigned long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<long long, long long>
{
public:
  using ValueType = long long;
};

template <>
class JoinTraits<long long, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<long long, float>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<long long, double>
{
public:
  using ValueType = double;
};

/** \class PixelTraits<unsigned long long>
 * Specializations for unsigned long long.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<unsigned long long, bool>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, char>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, unsigned char>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, short>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, unsigned short>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, int>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, unsigned int>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, unsigned long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, unsigned long long>
{
public:
  using ValueType = unsigned long long;
};

template <>
class JoinTraits<unsigned long long, float>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<unsigned long long, double>
{
public:
  using ValueType = double;
};


/**  \class PixelTraits<float>
 * Specializations for float.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<float, bool>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, char>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, unsigned char>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, short>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, unsigned short>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, int>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, unsigned int>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, unsigned long>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<float, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<float, float>
{
public:
  using ValueType = float;
};

template <>
class JoinTraits<float, double>
{
public:
  using ValueType = double;
};

/** \class PixelTraits<double>
 * Specializations for double.
 * \ingroup ITKCommon
 */
template <>
class JoinTraits<double, bool>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, char>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, unsigned char>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, short>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, unsigned short>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, int>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, unsigned int>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, unsigned long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, unsigned long long>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, float>
{
public:
  using ValueType = double;
};

template <>
class JoinTraits<double, double>
{
public:
  using ValueType = double;
};

/// \endcond

} // end namespace itk

#endif // itkPixelTraits_h
