/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsDiffusionTensor3DPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsDiffusionTensor3DPixel.h"

namespace itk
{
// All the specializations that were here previously have now been
// replaced with a single template in the header file.
//

//
// Helper macro for initializing the Zero and One static member of the
// NumericTraits<>.
//

#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

#define DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(T)                                                      \
  template< >                                                                                           \
  const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >::Zero = DiffusionTensor3D< T >( \
    NumericTraits< T >::Zero);                                                                          \
  template< >                                                                                           \
  const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >::One = DiffusionTensor3D< T >(  \
    NumericTraits< T >::One);

#else // ITK_USE_NUMERICTRAITS_PARTIAL_SPECIALIZATION

#define DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(T)                                                      \
  const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >::Zero = DiffusionTensor3D< T >( \
    NumericTraits< T >::Zero);                                                                          \
  const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >:: One = DiffusionTensor3D< T >( \
    NumericTraits< T >::One);

#endif // ITK_USE_NUMERICTRAITS_PARTIAL_SPECIALIZATION

//
// List here the specializations of the Traits:
//

DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(char);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned char);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(short);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned short);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(int);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned int);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(float);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(double);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(long long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned long long);
} // end namespace itk
