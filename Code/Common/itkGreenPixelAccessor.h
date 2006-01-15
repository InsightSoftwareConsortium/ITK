/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGreenPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGreenPixelAccessor_h
#define __itkGreenPixelAccessor_h


#include "itkRGBPixel.h"


namespace itk
{

/**
 * \class GreenPixelAccessor
 * \brief Give access to the Green component of a RGBPixel type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Green component.
 *
 * \sa ImageAdaptor
 *
 * \ingroup ImageAdaptors
 *
 */

template <class T>
class ITK_EXPORT GreenPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   GreenPixelAccessor        Self;
  
  /** External typedef. It defines the external aspect
   * that this class will exhibit */
  typedef T ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef     RGBPixel<T>   InternalType;

  /** Write access to the Green component */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { output.SetGreen( input ); }

  /** Read access to the Green component */
  inline const ExternalType & Get( const InternalType & input ) const
    { return input.GetGreen(); }

  bool operator!=( const Self & other ) const
  {
    return false;
  }

  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
};

  
} // end namespace itk

#endif

