/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBAPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsRGBAPixel_h
#define __itkNumericTraitsRGBAPixel_h

#include "itkNumericTraits.h"
#include "itkRGBAPixel.h"


namespace itk
{

//
// First we define a macro that can be customized to be used for a sequence of
// specializations or for a generic template instantiation. This Macro covers
// the implementation for good compilers and for Visual Studio 6.0.
//
#define itkNumericTraitsRGBAPixelMacro(T) \
template < _TEMPLATE_ARGUMENT_ >  \
class NumericTraits<RGBAPixel< T > >  \
{ \
public: \
  typedef T ValueType; \
 \
  typedef _TYPENAME_ NumericTraits<T>::AbsType        ElementAbsType; \
  typedef _TYPENAME_ NumericTraits<T>::AccumulateType ElementAccumulateType; \
  typedef _TYPENAME_ NumericTraits<T>::FloatType      ElementFloatType; \
  typedef _TYPENAME_ NumericTraits<T>::PrintType      ElementPrintType; \
  typedef _TYPENAME_ NumericTraits<T>::RealType       ElementRealType; \
 \
  typedef RGBAPixel<T>                   Self; \
 \
  typedef RGBAPixel<ElementAbsType>          AbsType; \
  typedef RGBAPixel<ElementAccumulateType>   AccumulateType; \
  typedef RGBAPixel<ElementFloatType>        FloatType; \
  typedef RGBAPixel<ElementPrintType>        PrintType; \
  typedef RGBAPixel<ElementRealType>         RealType; \
 \
  typedef ElementRealType ScalarRealType; \
 \
  static const RealType max( const Self & a ) \
    {  \
      RealType b(a.Size());  \
      b.Fill( NumericTraits< T >::max() ); \
      return b; \
    } \
  static const RealType min( const Self & a ) \
    {  \
      RealType b(a.Size());  \
      b.Fill( NumericTraits< T >::min() ); \
      return b; \
    } \
  static const Self ZeroValue() \
  {  \
    Self b; \
    b.Fill( NumericTraits< T >::Zero ); \
    return b; \
  } \
  static const Self OneValue() \
  {  \
    Self b; \
    b.Fill( NumericTraits< T >::One ); \
    return b; \
  } \
};


//
// Visual Studio 6.0 is not capable of managing the template implementation
// defined at the end of this file. Therefore we provide an alternative
// primitive implementation based on macros that define explicit
// instantiations.
//
#if defined( _MSC_VER ) && ( _MSC_VER < 1310 )

// These two symbols below are defined empty on purpose
#define _TYPENAME_
#define _TEMPLATE_ARGUMENT_

//
// List here the specializations of the Traits:
//
itkNumericTraitsRGBAPixelMacro( char );
itkNumericTraitsRGBAPixelMacro( unsigned char );
itkNumericTraitsRGBAPixelMacro( short );
itkNumericTraitsRGBAPixelMacro( unsigned short );
itkNumericTraitsRGBAPixelMacro( int );
itkNumericTraitsRGBAPixelMacro( unsigned int );
itkNumericTraitsRGBAPixelMacro( long );
itkNumericTraitsRGBAPixelMacro( unsigned long );
itkNumericTraitsRGBAPixelMacro( float );
itkNumericTraitsRGBAPixelMacro( double );

#else

// For all the other good compilers, we provide here a generic implementation
// based on creating types of RGBAPixels whose components are the types of the
// NumericTraits from the original RGBAPixel components. This implementation
// doesn't require specializations, since it is based on the concept that 
//
//    NumericTraits< RGBAPixle< T > >  is defined piecewise by
//    RGBAPixle< NumericTraits< T > >
//
//
// By defining the following symbols, the Macro above gets customized to become
// a generic template implementation of the traits
//
#define _TYPENAME_            typename
#define _TEMPLATE_ARGUMENT_   class T

//
// Then we simply call the macro once with the generic template argument T.
//
itkNumericTraitsRGBAPixelMacro( T );

#endif



//
// Finally, to avoid contamination of other files with the symbols defined
// here, we undefine the helper macros
//
#undef _TYPENAME_
#undef _TEMPLATE_ARGUMENT_


} // end namespace itk

#endif // __itkNumericTraitsRGBAPixel_h

