/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectProperty.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __SpatialObjectProperty_txx
#define __SpatialObjectProperty_txx

#include "itkSpatialObjectProperty.h"

namespace itk
{
  template< class TComponentType >
  const typename SpatialObjectProperty< TComponentType >::PixelType &
  SpatialObjectProperty< TComponentType >
  ::GetColor( void ) const
  { return m_Color; }

  template< class TComponentType >
  void 
  SpatialObjectProperty< TComponentType >
  ::SetColor( const PixelType & color )
  { m_Color = color; }

  template< class TComponentType >
    void
  SpatialObjectProperty< TComponentType >
  ::SetRed( TComponentType r )
  { m_Color.SetRed(r); }

  template< class TComponentType >
    TComponentType 
  SpatialObjectProperty< TComponentType >
  ::GetRed( void ) 
  const{ return m_Color.GetRed(); }

  template< class TComponentType >
  void 
  SpatialObjectProperty< TComponentType >
  ::SetGreen( TComponentType g )
  { 
    m_Color.SetGreen(g); 
  }

  template< class TComponentType >    
  TComponentType
  SpatialObjectProperty< TComponentType >
  ::GetGreen( void ) const{ return m_Color.GetGreen(); }

  template< class TComponentType >
  void 
  SpatialObjectProperty< TComponentType >
  ::SetBlue( TComponentType b ){ m_Color.SetBlue(b); }

  template< class TComponentType >
  TComponentType 
  SpatialObjectProperty< TComponentType >
  ::GetBlue( void ) const{ return m_Color.GetBlue(); }

  template< class TComponentType >
  void 
  SpatialObjectProperty< TComponentType >
  ::SetAlpha( TComponentType a){ m_Color.SetAlpha(a); }

  template< class TComponentType >
  TComponentType 
  SpatialObjectProperty< TComponentType >
  ::GetAlpha( void ) const{ return m_Color.GetAlpha(); }

  template< class TComponentType >
  void
  SpatialObjectProperty< TComponentType >
  ::SetName( char * name )
  { 
    m_Name.assign( name ); 
  }

  template< class TComponentType >
  typename SpatialObjectProperty< TComponentType >::StringType
  SpatialObjectProperty< TComponentType >
  ::GetName( void ) const
  { 
    return m_Name;
  }

 /*********************************************************
  * protected methods
  *********************************************************/

  template< class TComponentType >
  SpatialObjectProperty< TComponentType >
  ::SpatialObjectProperty()
  {
  }

  template< class TComponentType >
  SpatialObjectProperty< TComponentType >
  ::~SpatialObjectProperty()
  {
  }

  template< class TComponentType >
  void 
  SpatialObjectProperty< TComponentType >
  ::PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os,indent);
    os << indent << "Name: " << m_Name << std::endl;
    os << indent << "RGBA: "<< m_Color.GetRed() << " ";
    os << m_Color.GetGreen() << " ";
    os << m_Color.GetBlue() << std::endl;
  }

} // end of namespace itk

#endif // __SpatialObjectProperty_txx
