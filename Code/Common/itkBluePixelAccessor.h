/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBluePixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBluePixelAccessor_h
#define __itkBluePixelAccessor_h


#include "itkRGBPixel.h"


namespace itk
{

/**
 * \class BluePixelAccessor
 * \brief Give access to the Blue component of a RGBPixel type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Blue component.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 *
 */

template <class T>
class ITK_EXPORT BluePixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   BluePixelAccessor        Self;

 /** External typedef. It defines the external aspect
   * that this class will exhibit */
  typedef T ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef   RGBPixel<T>    InternalType;

  /** Write access to the Blue component */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { output.SetBlue( input ); }

  /** Read access to the Blue component */
  inline const ExternalType & Get( const InternalType & input ) const
    { return input.GetBlue(); }

  bool operator!=( const Self & other ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
};
  
}  // end namespace itk

#endif

