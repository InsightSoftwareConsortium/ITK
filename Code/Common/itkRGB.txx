/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGB.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRGB.h"

namespace itk
{

/**
 * Default constructor.
 */
template < typename TComponent >
RGB< TComponent >
::RGB()
{
}


/**
 * Copy Constructor 
 */
template < typename TComponent >
RGB< TComponent >
::RGB(const RGB<TComponent> & rgb )
{
  m_Red     = rgb.m_Red;
  m_Green   = rgb.m_Green;
  m_Blue    = rgb.m_Blue;
}

/**
 *  Assignment Operator
 */
template < typename TComponent >
const RGB<TComponent> &
RGB< TComponent >
::operator=(const RGB<TComponent> & rgb )
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
RGB< TComponent >
::RGB( TComponent red, TComponent green, TComponent blue )
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
RGB< TComponent >
::SetRed( TComponent red )
{
  m_Red = red;
}



/**
 *  Set Green
 */
template < typename TComponent >
void
RGB< TComponent >
::SetGreen( TComponent green )
{
  m_Green = green;
}


/**
 *  Set Blue
 */
template < typename TComponent >
void
RGB< TComponent >
::SetBlue( TComponent blue )
{
  m_Blue = blue;
}



/**
 *  Set the three components
 */
template < typename TComponent >
void
RGB< TComponent >
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
TComponent 
RGB< TComponent >
::GetRed( void ) const
{
  return m_Red;
}



/**
 *  Get Green
 */
template < typename TComponent >
TComponent 
RGB< TComponent >
::GetGreen( void ) const
{
  return m_Green;
}



/**
 *  Get Blue
 */
template < typename TComponent >
TComponent 
RGB< TComponent >
::GetBlue( void ) const
{
  return m_Blue;
}



} // end namespace itk
