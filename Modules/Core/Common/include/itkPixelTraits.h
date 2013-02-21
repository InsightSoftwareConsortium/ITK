/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef __itkPixelTraits_h
#define __itkPixelTraits_h

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
template< class TPixelType >
class PixelTraits
{
public:
  /** Dimension of the pixel (range). */
  itkStaticConstMacro(Dimension, unsigned int, TPixelType::Length);

  /** Type of a single component of a pixel. */
  typedef typename TPixelType::ValueType ValueType;
};

/** \cond HIDE_SPECIALIZATION_DOCUMENTATION */

/** \class PixelTraits<bool>
 * Specialization of PixelTraits for scalar images.
 * \ingroup ITKCommon
 */
template< >
class PixelTraits< bool >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef bool ValueType;
};

template< >
class PixelTraits< char >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef char ValueType;
};

template< >
class PixelTraits< int8_t >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef char ValueType;
};

template< >
class PixelTraits< uint8_t >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef uint8_t ValueType;
};

template< >
class PixelTraits< short >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef short ValueType;
};

template< >
class PixelTraits< uint16_t >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef uint16_t ValueType;
};

template< >
class PixelTraits< int >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef int ValueType;
};

template< >
class PixelTraits< unsigned int >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef unsigned int ValueType;
};

template< >
class PixelTraits< long >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef long ValueType;
};

template< >
class PixelTraits< unsigned long >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef unsigned long ValueType;
};

template< >
class PixelTraits< float >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef float ValueType;
};

template< >
class PixelTraits< double >
{
public:
  itkStaticConstMacro(Dimension, unsigned int, 1);
  typedef double ValueType;
};

/** \endcond */

/** \class JoinTraits
 * \brief Trait to determine what datatype is needed if the specified
 * pixel types are "joined" into a single vector.
 *
 * JoinTraits defines the value type needed to combine the specified
 * pixel types into a single vector.  The data type selected is the
 * smallest data type that can represent the dynamic range of both
 * input pixel types.  For example, if a char and uint16_t are
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
template< class TValueType1, class TValueType2 >
class JoinTraits
{
public:
  typedef TValueType1 ValueType;
};

/** \cond HIDE_SPECIALIZATION_DOCUMENTATION */

/** \class JoinTraits
 * Specializations for bool.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< bool, bool >
{
public:
  typedef bool ValueType;
};

template< >
class JoinTraits< bool, char >
{
public:
  typedef char ValueType;
};

template< >
class JoinTraits< bool, uint8_t >
{
public:
  typedef uint8_t ValueType;
};

template< >
class JoinTraits< bool, short >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< bool, uint16_t >
{
public:
  typedef uint16_t ValueType;
};

template< >
class JoinTraits< bool, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< bool, unsigned int >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< bool, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< bool, unsigned long >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< bool, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< bool, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<char>
 * Specializations for char.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< char, bool >
{
public:
  typedef char ValueType;
};

template< >
class JoinTraits< char, char >
{
public:
  typedef char ValueType;
};

template< >
class JoinTraits< char, uint8_t >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< char, short >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< char, uint16_t >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< char, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< char, unsigned int >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< char, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< char, unsigned long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< char, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< char, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<uint8_t>
 * Specializations for uint8_t.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< uint8_t, bool >
{
public:
  typedef uint8_t ValueType;
};

template< >
class JoinTraits< uint8_t, char >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< uint8_t, uint8_t >
{
public:
  typedef uint8_t ValueType;
};

template< >
class JoinTraits< uint8_t, short >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< uint8_t, uint16_t >
{
public:
  typedef uint16_t ValueType;
};

template< >
class JoinTraits< uint8_t, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< uint8_t, unsigned int >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< uint8_t, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< uint8_t, unsigned long >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< uint8_t, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< uint8_t, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<short>
 * Specializations for short.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< short, bool >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< short, char >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< short, uint8_t >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< short, short >
{
public:
  typedef short ValueType;
};

template< >
class JoinTraits< short, uint16_t >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< short, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< short, unsigned int >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< short, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< short, unsigned long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< short, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< short, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<uint16_t>
 * Specializations for uint16_t.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< uint16_t, bool >
{
public:
  typedef uint16_t ValueType;
};

template< >
class JoinTraits< uint16_t, char >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< uint16_t, uint8_t >
{
public:
  typedef uint16_t ValueType;
};

template< >
class JoinTraits< uint16_t, short >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< uint16_t, uint16_t >
{
public:
  typedef uint16_t ValueType;
};

template< >
class JoinTraits< uint16_t, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< uint16_t, unsigned int >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< uint16_t, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< uint16_t, unsigned long >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< uint16_t, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< uint16_t, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<int>
 * Specializations for int.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< int, bool >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, char >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, uint8_t >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, short >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, uint16_t >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, int >
{
public:
  typedef int ValueType;
};

template< >
class JoinTraits< int, unsigned int >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< int, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< int, unsigned long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< int, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< int, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<unsigned int>
 * Specializations for unsigned int.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< unsigned int, bool >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< unsigned int, char >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned int, uint8_t >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< unsigned int, short >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned int, uint16_t >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< unsigned int, int >
{
public:
  // unsigned int & unsigned long may be the same size, so promote to float
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned int, unsigned int >
{
public:
  typedef unsigned int ValueType;
};

template< >
class JoinTraits< unsigned int, long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned int, unsigned long >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned int, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned int, double >
{
public:
  typedef double ValueType;
};

/** \class PixelTraits<long>
 * Specializations for long.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< long, bool >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, char >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, uint8_t >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, short >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, uint16_t >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, int >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, unsigned int >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< long, long >
{
public:
  typedef long ValueType;
};

template< >
class JoinTraits< long, unsigned long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< long, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< long, double >
{
public:
  typedef double ValueType;
};

/** \class PixelTraits<unsigned long>
 * Specializations for unsigned long.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< unsigned long, bool >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned long, char >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned long, uint8_t >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned long, short >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned long, uint16_t >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned long, int >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned long, unsigned int >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned long, long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned long, unsigned long >
{
public:
  typedef unsigned long ValueType;
};

template< >
class JoinTraits< unsigned long, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< unsigned long, double >
{
public:
  typedef double ValueType;
};

/**  \class PixelTraits<float>
 * Specializations for float.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< float, bool >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, char >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, uint8_t >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, short >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, uint16_t >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, int >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, unsigned int >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, unsigned long >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, float >
{
public:
  typedef float ValueType;
};

template< >
class JoinTraits< float, double >
{
public:
  typedef double ValueType;
};

/** \class PixelTraits<double>
 * Specializations for double.
 * \ingroup ITKCommon
 */
template< >
class JoinTraits< double, bool >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, char >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, uint8_t >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, short >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, uint16_t >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, int >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, unsigned int >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, long >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, unsigned long >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, float >
{
public:
  typedef double ValueType;
};

template< >
class JoinTraits< double, double >
{
public:
  typedef double ValueType;
};

/** \endcond */

} // end namespace itk

#endif // __itkPixelTraits_h
