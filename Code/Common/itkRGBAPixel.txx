/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBAPixel.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRGBAPixel_txx
#define _itkRGBAPixel_txx
#include "itkRGBAPixel.h"
#include "itkNumericTraits.h"

namespace itk
{

  /*
 * Assignment Operator
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Base Array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Const Base Array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assigment from a plain array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const ComponentType r[4])
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a const scalar value
 */
template<class T>
typename RGBAPixel<T>::ArrayCommaListCopier
RGBAPixel<T>
::operator= (const ComponentType& r)
{
  return BaseArray::operator=(r);
}

/**
 * Print content to an ostream
 */
template<class TComponent>
std::ostream &
operator<<(std::ostream& os,const RGBAPixel<TComponent> & c ) 
{
  os <<  c[0] << "  ";
  os <<  c[1] << "  ";
  os <<  c[2] << "  ";
  os <<  c[3] ;
  return os;
}


/**
 * Read content from an istream
 */
template<class TComponent>
std::istream &
operator>>(std::istream& is, RGBAPixel<TComponent> & c ) 
{
  TComponent red;
  TComponent green;
  TComponent blue;
  TComponent alpha;
  is >> red >> green >> blue;
  c.SetRed( red );
  c.SetGreen( green );
  c.SetBlue( blue );
  c.SetAlpha( alpha );
  return is;
}

} // end namespace itk

#endif
