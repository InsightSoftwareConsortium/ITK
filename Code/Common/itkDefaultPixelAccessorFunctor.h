/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultPixelAccessorFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDefaultPixelAccessorFunctor_h
#define __itkDefaultPixelAccessorFunctor_h

#include "itkMacro.h"

namespace itk
{
/** \class DefaultPixelAccessorFunctor
 * \brief This class provides a common API for pixel accessors for Image and 
 * VectorImage. (between the DefaultVectorPixelAccessor and DefaultPixelAccessor).
 * 
 * The pixel accessor is set with the SetPixelAccessor method. This accessor is 
 * meant to be used for Image and not for VectorImage. 
 *
 * \thanks
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 * 
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultPixelAccessor
 * \sa DefaultVectorPixelAccessorFunctor
 */
template <class TImageType >
class ITK_EXPORT DefaultPixelAccessorFunctor
{
public:
  typedef TImageType                                   ImageType;
  typedef typename ImageType::InternalPixelType        InternalPixelType;
  typedef typename ImageType::PixelType                ExternalPixelType;
  typedef typename ImageType::AccessorType             PixelAccessorType;
  typedef unsigned int                                 VectorLengthType;
  
  static void SetVectorLength( ImageType *image, VectorLengthType length )
    { 
    }

  static VectorLengthType GetVectorLength( const ImageType * itkNotUsed(image) )
    {
    return 1;
    } 
  
  /** Set the PixelAccessor. This is set at construction time by the image iterators. 
   * The type PixelAccessorType is obtained from the ImageType over which the iterators
   * are templated.
   * */
  inline void SetPixelAccessor( PixelAccessorType& accessor ) 
    {
    m_PixelAccessor = accessor;
    }

  /** Set the pointer index to the start of the buffer. 
   * The method exists to maintain consistency in the API of the 
   * DefaultPixelAccessorFunctor and the DefaultVectorPixelAccessorFunctor. */
  inline void SetBegin( const InternalPixelType *itkNotUsed(begin) ) {};
  
  /** Set output using the value in input */
  inline void Set( InternalPixelType & output, const ExternalPixelType &input ) const
    {
    m_PixelAccessor.Set( output, input );
    }

  /** Get the value from input */
  inline ExternalPixelType Get( InternalPixelType &input ) const
    {
    return m_PixelAccessor.Get( input );
    }

  /** Get a const reference to the pixel. */
  inline const ExternalPixelType Get( const InternalPixelType & input ) const
    {
    return m_PixelAccessor.Get( input );
    }
  
private:
  PixelAccessorType m_PixelAccessor; // The pixel accessor
};

}

#endif    
