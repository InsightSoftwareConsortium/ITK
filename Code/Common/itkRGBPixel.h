/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRGBPixel_h
#define __itkRGBPixel_h

// Undefine an eventual RGBPixel macro
#ifdef RGBPixel
#undef RGBPixel
#endif

#include <itkIndent.h>

namespace itk
{

/** \class RGBPixel
 * \brief Represent Red, Green and Blue component for color images
 *
 * This class is templated over the representation used for each
 * component
 *
 */

template < typename TComponent = unsigned short >
class RGBPixel
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGBPixel  Self;

  /**
   *  Define the component type
   */
  typedef   TComponent    ComponentType;

  /**
   * Constructors.
   */
  inline RGBPixel();
  inline RGBPixel(const RGBPixel &);
  inline RGBPixel( ComponentType red, ComponentType green, ComponentType blue );
  
 
  /**
   * Assignment Operator
   */
  inline const RGBPixel & operator=( const RGBPixel & );

  /**
   * Set the Red component
   */
  inline void SetRed( ComponentType red );

  /**
   * Set the Green component
   */
  inline void SetGreen( ComponentType green );

  /**
   * Set the Blue component
   */
  inline void SetBlue( ComponentType blue );

  /**
   * Set the three components
   */
  inline void Set( ComponentType red, ComponentType green, ComponentType blue );

  /**
   * Get the Red component
   */
  inline const ComponentType & GetRed( void ) const;
  
  /**
   * Get the Green component
   */
  inline const ComponentType & GetGreen( void ) const;
  
  /**
   * Get the Blue component
   */
  inline const ComponentType & GetBlue( void ) const;

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


template< typename TComponent  >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const RGBPixel<TComponent> & c); 

template< typename TComponent  >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                          RGBPixel<TComponent> & c); 




} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBPixel.txx"
#endif

#endif
