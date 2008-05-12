/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{

//
// Helper macro for defining the traits of the Vector type for a given component type and dimension
//
#define VECTORSTATICTRAITSMACRO( T, N ) \
const Vector<T,N>  NumericTraits<Vector<T,N> >::Zero = Vector<T,N>( NumericTraits<T>::Zero ); \
const Vector<T,N>  NumericTraits<Vector<T,N> >::One  = Vector<T,N>( NumericTraits<T>::One ); \
Vector<T,N>  NumericTraits<Vector<T,N> >::ZeroValue() { return NumericTraits<Vector<T,N> >::Zero; }

//
// Helper macro for defining the traits of Vector<T,k> for k in [1:9]
//
#define VECTORSTATICTRAITSMACROFORONETYPE(T) \
VECTORSTATICTRAITSMACRO(T,1 ); \
VECTORSTATICTRAITSMACRO(T,2 ); \
VECTORSTATICTRAITSMACRO(T,3 ); \
VECTORSTATICTRAITSMACRO(T,4 ); \
VECTORSTATICTRAITSMACRO(T,5 ); \
VECTORSTATICTRAITSMACRO(T,6 ); \
VECTORSTATICTRAITSMACRO(T,7 ); \
VECTORSTATICTRAITSMACRO(T,8 ); \
VECTORSTATICTRAITSMACRO(T,9 ); 

//
// Calls to macros for defining the traits of Vector for different component types
//
VECTORSTATICTRAITSMACROFORONETYPE( unsigned char );
VECTORSTATICTRAITSMACROFORONETYPE( signed char );
VECTORSTATICTRAITSMACROFORONETYPE( char );
VECTORSTATICTRAITSMACROFORONETYPE( short );
VECTORSTATICTRAITSMACROFORONETYPE( unsigned short );
VECTORSTATICTRAITSMACROFORONETYPE( int );
VECTORSTATICTRAITSMACROFORONETYPE( unsigned int );
VECTORSTATICTRAITSMACROFORONETYPE( long );
VECTORSTATICTRAITSMACROFORONETYPE( unsigned long );
VECTORSTATICTRAITSMACROFORONETYPE( float );
VECTORSTATICTRAITSMACROFORONETYPE( double );
VECTORSTATICTRAITSMACROFORONETYPE( long double );

} // end namespace itk
