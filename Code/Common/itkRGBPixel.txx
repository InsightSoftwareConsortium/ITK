/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
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
