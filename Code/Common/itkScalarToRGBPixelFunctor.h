/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToRGBPixelFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarToRGBPixelFunctor_h
#define _itkScalarToRGBPixelFunctor_h

#include "itkRGBPixel.h"

namespace itk {

namespace Functor {  
  
template< class TScalar >
class ScalarToRGBPixelFunctor
{
public:
  ScalarToRGBPixelFunctor();
  ~ScalarToRGBPixelFunctor() {};

  typedef unsigned char               RGBComponentType;
  typedef RGBPixel<RGBComponentType>  RGBPixelType;
  typedef TScalar                     ScalarType;
  
  RGBPixelType operator()( const TScalar &) const;

  void SetLittleEndian()
  {
    m_IsBigEndian = false;
  }
  void SetBigEndian()
  {
    m_IsBigEndian = true;
  }
  
private:
  bool m_IsBigEndian;
  ::size_t m_Index[3];
  
};
  
} // end namespace functor

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarToRGBPixelFunctor.txx"
#endif

#endif
