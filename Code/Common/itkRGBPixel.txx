/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkRGBPixel_txx
#define _itkRGBPixel_txx
#include "itkRGBPixel.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 * Default constructor.
 */
template < typename TComponent >
RGBPixel< TComponent >
::RGBPixel()
{
  m_Red = NumericTraits<TComponent>::Zero;
  m_Green = NumericTraits<TComponent>::Zero;
  m_Blue = NumericTraits<TComponent>::Zero;
}


/**
 * Copy Constructor 
 */
template < typename TComponent >
RGBPixel< TComponent >
::RGBPixel(const RGBPixel<TComponent> & rgb )
{
  m_Red     = rgb.m_Red;
  m_Green   = rgb.m_Green;
  m_Blue    = rgb.m_Blue;
}

/**
 *  Assignment Operator
 */
template < typename TComponent >
const RGBPixel<TComponent> &
RGBPixel< TComponent >
::operator=(const RGBPixel<TComponent> & rgb )
{
  m_Red     = rgb.m_Red;
  m_Green   = rgb.m_Green;
  m_Blue    = rgb.m_Blue;
  return *this;
}


/**
 * Constructor with components
 */
template < typename TComponent >
RGBPixel< TComponent >
::RGBPixel( TComponent red, TComponent green, TComponent blue )
{
  m_Red   = red;
  m_Green = green;
  m_Blue  = blue;
}


/**
 *  Set Red
 */
template < typename TComponent >
void
RGBPixel< TComponent >
::SetRed( TComponent red )
{
  m_Red = red;
}



/**
 *  Set Green
 */
template < typename TComponent >
void
RGBPixel< TComponent >
::SetGreen( TComponent green )
{
  m_Green = green;
}


/**
 *  Set Blue
 */
template < typename TComponent >
void
RGBPixel< TComponent >
::SetBlue( TComponent blue )
{
  m_Blue = blue;
}



/**
 *  Set the three components
 */
template < typename TComponent >
void
RGBPixel< TComponent >
::Set( TComponent red, TComponent green, TComponent blue )
{
  m_Red   = red;
  m_Green = green;
  m_Blue  = blue;
}



/**
 *  Get Red
 */
template < typename TComponent >
const TComponent &
RGBPixel< TComponent >
::GetRed( void ) const
{
  return m_Red;
}



/**
 *  Get Green
 */
template < typename TComponent >
const TComponent &
RGBPixel< TComponent >
::GetGreen( void ) const
{
  return m_Green;
}



/**
 *  Get Blue
 */
template < typename TComponent >
const TComponent &
RGBPixel< TComponent >
::GetBlue( void ) const
{
  return m_Blue;
}


/**
 * Print content to an ostream
 */
template<class TComponent>
std::ostream &
operator<<(std::ostream& os,const RGBPixel<TComponent> & c ) 
{
  os <<  c.GetRed() << "  ";
  os <<  c.GetGreen() << "  ";
  os <<  c.GetBlue();
  return os;
}


/**
 * Read content from an istream
 */
template<class TComponent>
std::istream &
operator>>(std::istream& is, RGBPixel<TComponent> & c ) 
{
  TComponent tmp;
  
  is >> tmp;
  c.SetRed( tmp );
  
  is >> tmp;
  c.SetGreen( tmp );
  
  is >> tmp;
  c.SetBlue( tmp );

  return is;
}




} // end namespace itk

#endif
