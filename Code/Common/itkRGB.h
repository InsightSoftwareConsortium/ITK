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

#include "itkMacro.h"

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
  
 
  /**
   * Operators
   */
  const RGB & operator=( const RGB & );

  /**
   * Access routines.
   */
  void SetRed( ComponentType red );
  void SetGreen( ComponentType green );
  void SetBlue( ComponentType blue );

  ComponentType GetRed( void ) const;
  ComponentType GetGreen( void ) const;
  ComponentType GetBlue( void ) const;

private:
  /**
   * Color components
   */
  ComponentType  m_Red;
  ComponentType  m_Green;
  ComponentType  m_Blue;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGB.txx"
#endif

#endif
