/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGB.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRGB_h
#define __itkRGB_h

// Undefine an eventual RGB macro
#ifdef RGB
#undef RGB
#endif

namespace itk
{

/** \class RGB
 * \brief Represent Red, Green and Blue component for color images
 *
 * This class is templated over the representation used for each
 * component
 *
 */

template < typename TComponent = unsigned short >
class RGB
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGB  Self;

  /**
   *  Define the component type
   */
  typedef   TComponent    ComponentType;

  /**
   * Constructors.
   */
  RGB();
  RGB(const RGB &);
  RGB( ComponentType red, ComponentType green, ComponentType blue );
  
 
  /**
   * Assignment Operator
   */
  const RGB & operator=( const RGB & );

  /**
   * Set the Red component
   */
  void SetRed( ComponentType red );

  /**
   * Set the Green component
   */
  void SetGreen( ComponentType green );

  /**
   * Set the Blue component
   */
  void SetBlue( ComponentType blue );

  /**
   * Set the three components
   */
  void Set( ComponentType red, ComponentType green, ComponentType blue );

  /**
   * Get the Red component
   */
  ComponentType GetRed( void ) const;
  
  /**
   * Get the Green component
   */
  ComponentType GetGreen( void ) const;
  
  /**
   * Get the Blue component
   */
  ComponentType GetBlue( void ) const;

private:
  /**
   * Red component
   */
  ComponentType  m_Red;
  
  /**
   * Green component
   */
  ComponentType  m_Green;
  
  /**
   * Blue component
   */
  ComponentType  m_Blue;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGB.txx"
#endif

#endif
