/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultConvertPixelTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,1)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,float,6)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,double,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,double,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,double,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,double,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector,double,6)

  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,1)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,float,6)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,double,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,double,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,double,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,double,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point,double,6)

  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,1)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,float,6)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,double,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,double,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,double,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,double,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector,double,6)

  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,1)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,float,6)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,double,2)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,double,3)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,double,4)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,double,5)
  ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray,double,6)

//
//  End of Traits for the classes deriving from FixedArray.
//
//


  
  } // end namespace itk


#endif
