/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectProperty.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __SpatialObjectProperty_h
#define __SpatialObjectProperty_h

#include <string>

#include "itkIndent.h"
#include "itkRGBAPixel.h"
#include "itkLightObject.h"

namespace itk{

/**
 * This class contains the objects properties such as colors, opacity, etc...
 * it's templated over the representation to use for each color component. 
 */
 
  template< class TComponentType = float >
  class ITK_EXPORT SpatialObjectProperty 
  : public LightObject 
  {
  public:

    typedef SpatialObjectProperty< TComponentType > Self;
    typedef LightObject Superclass;
    typedef RGBAPixel< TComponentType > PixelType;
    typedef std::string StringType;

    typedef SmartPointer<Self> Pointer;
    typedef SmartPointer<const Self> ConstPointer;

    itkNewMacro( Self );
    itkTypeMacro( Self, Superclass );

    const PixelType & GetColor( void ) const;
    void SetColor(const PixelType & color );

    void SetRed( TComponentType r );
    TComponentType GetRed( void ) const;

    void SetGreen( TComponentType g );
    TComponentType GetGreen( void ) const;

    void SetBlue( TComponentType b );
    TComponentType GetBlue( void ) const;

    void SetAlpha( TComponentType a);
    TComponentType GetAlpha( void ) const;

    SpatialObjectProperty();
    virtual ~SpatialObjectProperty();

    void SetName( char * name );
    StringType GetName( void ) const;

    unsigned long GetMTime(void){return m_MTime;}

  protected:

    void PrintSelf(std::ostream &os, Indent indent) const;
    void Modified(void){m_MTime++;}

  private:    

    PixelType m_Color;
    StringType m_Name;
    unsigned long m_MTime;

  };

}



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectProperty.txx"
#endif

#endif // __SpatialObjectProperty_h
