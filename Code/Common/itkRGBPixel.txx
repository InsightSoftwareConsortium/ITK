/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRGBPixel_txx
#define _itkRGBPixel_txx
#include "itkRGBPixel.h"
#include "itkNumericTraits.h"

namespace itk
{

  /*
 * Assignment Operator
 */
template<class T>
RGBPixel<T>&
RGBPixel<T>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assigment from a plain array
 */
template<class T>
RGBPixel<T>&
RGBPixel<T>
::operator= (const ComponentType r[3])
{
  BaseArray::operator=(r);
  return *this;
}


  
/**
 * Returns a temporary copy of a vector
 */
template<class T>
RGBPixel<T> 
RGBPixel<T>
::operator+(const Self & r) const
{
  Self result;
  for( unsigned int i=0; i<3; i++) 
  {
    result[i] = (*this)[i] + r[i];
  }
  return result;
}




/**
 * Returns a temporary copy of a vector
 */
template<class T>
RGBPixel<T> 
RGBPixel<T>
::operator-(const Self & r) const
{
  Self result;
  for( unsigned int i=0; i<3; i++) 
  {
    result[i] = (*this)[i] - r[i];
  }
  return result;
}


 
/**
 * Returns a temporary copy of a vector
 */
template<class T>
const RGBPixel<T> & 
RGBPixel<T>
::operator+=(const Self & r) 
{
  for( unsigned int i=0; i<3; i++) 
  {
    (*this)[i] += r[i];
  }
  return *this;
}



 
/**
 * Returns a temporary copy of a vector
 */
template<class T>
const RGBPixel<T> & 
RGBPixel<T>
::operator-=(const Self & r)
{
  for( unsigned int i=0; i<3; i++) 
  {
    (*this)[i] -= r[i];
  }
  return *this;
}





/**
 * Returns a temporary copy of a vector
 */
template<class T>
RGBPixel<T> 
RGBPixel<T>
::operator*(const ComponentType & r) const
{
  Self result;
  for( unsigned int i=0; i<3; i++) 
  {
    result[i] = (*this)[i] * r;
  }
  return result;
}






/**
 * Print content to an ostream
 */
template<class TComponent>
std::ostream &
operator<<(std::ostream& os,const RGBPixel<TComponent> & c ) 
{
  os <<  c[0] << "  ";
  os <<  c[1] << "  ";
  os <<  c[2] ;
  return os;
}


/**
 * Read content from an istream
 */
template<class TComponent>
std::istream &
operator>>(std::istream& is, RGBPixel<TComponent> & c ) 
{
  TComponent red;
  TComponent green;
  TComponent blue;
  is >> red >> green >> blue;
  c.SetRed( red );
  c.SetGreen( green );
  c.SetBlue( blue );
  return is;
}

} // end namespace itk

#endif
