/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBToVectorPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBToVectorPixelAccessor_h
#define __itkRGBToVectorPixelAccessor_h


#include "itkRGBPixel.h"
#include "itkVector.h"


namespace itk
{
namespace Accessor
{
/**
 * \class RGBToVectorPixelAccessor
 * \brief Give access to a RGBPixel as if it were a Vector type.
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make an RGBPixel image appear as being
 * an image of Vector pixel type.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 *
 */

template <class T>
class ITK_EXPORT RGBToVectorPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   RGBToVectorPixelAccessor        Self;

 /** External typedef. It defines the external aspect
   * that this class will exhibit */
  typedef  Vector<T,3>     ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef   RGBPixel<T>    InternalType;

  /** Write access to the RGBToVector component */
  inline void Set( InternalType & output, const ExternalType & input ) const
    { 
    output[0] = input[0];
    output[1] = input[1];
    output[2] = input[2];
    }

  /** Read access to the RGBToVector component */
  inline const ExternalType & Get( const InternalType & input ) const
    { 
    m_Temp[0] = input[0];  
    m_Temp[1] = input[1];  
    m_Temp[2] = input[2];  
    return m_Temp;
    }

private:
  mutable ExternalType m_Temp;
};
  
}  // end namespace Accessor
}  // end namespace itk

#endif

