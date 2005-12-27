/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultConvertPixelTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkDefaultConvertPixelTraits_h
#define __itkDefaultConvertPixelTraits_h

#include "itkOffset.h"
#include "itkCovariantVector.h"
#include "itkVector.h"
#include "itkPoint.h"

namespace itk
{
  
/** \brief Traits class used to by ConvertPixels to convert blocks of pixels.
 *
 *  TOutputPixelType is the destination type. The input type is infered
 *  by the templated static function Convert.
 *
 *  This implementaion, does a simple assignment operator, so if you are
 *  going from from a higher bit representation to a lower bit one (int to
 *  char), you may want to specialize and add some sort of transfer function.
 */
template<typename PixelType>
class DefaultConvertPixelTraits
{
public:
  /** Determine the pixel data type. */
  typedef typename PixelType::ComponentType ComponentType;

  /** Return the number of components per pixel. */
  static unsigned int GetNumberOfComponents() 
  { return PixelType::GetNumberOfComponents();}
  
  /** Return the nth component of the pixel. */
  static ComponentType GetNthComponent(int c, const PixelType& pixel) 
  { return pixel.GetNthComponent(c); }

  /** Set the nth component of the pixel. */
  static void SetNthComponent(int c, PixelType& pixel, const ComponentType& v) 
  { pixel.SetNthComponent(c, v); }

  /** Return a single scalar value from this pixel. */
  static ComponentType GetScalarValue(const PixelType& pixel)
  { return pixel.GetScalarValue(); }

};

#define ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(type)                    \
template<>                                                               \
class DefaultConvertPixelTraits<type>                                    \
{                                                                        \
public:                                                                  \
  typedef type ComponentType;                                            \
  static unsigned int GetNumberOfComponents()                            \
    {                                                                    \
      return 1;                                                          \
    }                                                                    \
  static void SetNthComponent(int , type& pixel, const ComponentType& v) \
    {                                                                    \
      pixel = v;                                                         \
    }                                                                    \
  static type GetScalarValue(const type& pixel)                          \
    {                                                                    \
      return pixel;                                                      \
    }                                                                    \
};


ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(float)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(double)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(int)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(char)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(short)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned int)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned char)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned short)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(long)  
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned long)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(bool)

#undef ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL




//
//  Default traits for the Offset<> pixel type
//

#define ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(dimension)                  \
template<>                                                               \
class DefaultConvertPixelTraits< Offset<dimension> >                     \
{                                                                        \
public:                                                                  \
  typedef Offset<dimension>  TargetType;                                 \
  typedef TargetType::OffsetValueType  ComponentType;                    \
  static unsigned int GetNumberOfComponents()                            \
    {                                                                    \
      return dimension;                                                  \
    }                                                                    \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
    {                                                                    \
      pixel[i] = v;                                                      \
    }                                                                    \
  static ComponentType GetScalarValue(const TargetType& pixel)           \
    {                                                                    \
      return pixel[0];                                                   \
    }                                                                    \
};                                                                       \


// Define traits for Offset<> from dimensions 1 to 5
  ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(1)
  ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(2)
  ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(3)
  ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(4)
  ITK_DEFAULTCONVERTTRAITS_OFFSET_TYPE(5)




//
//  Default traits for the pixel types deriving from FixedArray<>
//

#define ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(type,componenttype, dimension) \
template<>                                                               \
class DefaultConvertPixelTraits< type< componenttype, dimension> >       \
{                                                                        \
public:                                                                  \
  typedef type< componenttype, dimension >  TargetType;                  \
  typedef componenttype                     ComponentType;               \
  static unsigned int GetNumberOfComponents()                            \
    {                                                                    \
      return dimension;                                                  \
    }                                                                    \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
    {                                                                    \
      pixel[i] = v;                                                      \
    }                                                                    \
  static ComponentType GetScalarValue(const TargetType& pixel)           \
    {                                                                    \
      return pixel[0];                                                   \
    }                                                                    \
};                                                                       \





//
//
// Define traits for Classed deriving from FixedArray from dimensions 1 to 6
// These classes include: Vector, CovariantVector and Point.
//
//
#define ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, Type) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,1) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,2) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,3) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,4) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,5) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,6)

#define ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(ArrayType) \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, char); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned char); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, short); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned short); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, int); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned int); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, long); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned long); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, float); \
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, double);

  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(Vector);
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(CovariantVector);
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(Point);
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(FixedArray);

//
//  End of Traits for the classes deriving from FixedArray.
//
//




//
//  Default traits for the pixel types deriving from std::complex<>
//

#define ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE( componenttype ) \
template<>                                                               \
class DefaultConvertPixelTraits< ::std::complex< componenttype > >       \
{                                                                        \
public:                                                                  \
  typedef ::std::complex< componenttype>  TargetType;                    \
  typedef componenttype                     ComponentType;               \
  static unsigned int GetNumberOfComponents()                            \
    {                                                                    \
    return 2;                                                  \
    }                                                                    \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
    {                                                                    \
    if( i == 0 )                                                            \
      {                                                                  \
      pixel = TargetType( v, pixel.imag() );                           \
      }                                                                  \
    else                                                                 \
      {                                                                  \
      pixel = TargetType( pixel.real(), v );                           \
      }                                                                  \
    }                                                                    \
  static ComponentType GetScalarValue(const TargetType& pixel)           \
    {                                                                    \
    return std::norm(pixel);                                             \
    }                                                                    \
};                                                                       \

ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(float);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(double);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(signed int);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(unsigned int);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(signed char);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(unsigned char);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(signed long);
ITK_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(unsigned long);

//
//  End of Traits for the classes deriving from std::complex.
//
//




  
  } // end namespace itk


#endif
