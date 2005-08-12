/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultVectorPixelAccessorFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDefaultVectorPixelAccessorFunctor_h
#define __itkDefaultVectorPixelAccessorFunctor_h

namespace itk
{
/** \class DefaultVectorPixelAccessorFunctor
 * \brief This class provides a common API for pixel accessors for Image and 
 * VectorImage. (between the DefaultVectorPixelAccessor and DefaultPixelAccessor).
 * 
 * The pixel accessor is set with the SetPixelAccessor method. This accessor is 
 * meant to be used only for VectorImage and not for Image. Prior to use, the
 * start of the VectorImage buffer must also be set with the SetBegin method.
 *
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultPixelAccessor
 * \sa DefaultPixelAccessorFunctor
 * 
 * \thanks
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 */
template <class TInternalType, class TExternalType, class TPixelAccessor  >
class ITK_EXPORT DefaultVectorPixelAccessorFunctor
{
public:
  /** Set the PixelAccessor. This is set at construction time by the image iterators. 
   * The type TPixelAccessor is obtained from the ImageType over which the iterators
   * are templated.
   * */
  inline void SetPixelAccessor( TPixelAccessor& accessor ) 
    {
    m_PixelAccessor = accessor;
    }
  
  /** Set the pointer index to the start of the buffer. */
  inline void SetBegin( const TInternalType * begin ) 
    { this->m_Begin = const_cast< TInternalType * >( begin ); }
  
  /** Set output using the value in input */
  inline void Set( TInternalType & output, const TExternalType &input ) const
    {
    m_PixelAccessor.Set( output, input, (&output)-m_Begin );
    }

  /** Get the value from input */
  inline TExternalType Get( const TInternalType &input ) const
    {
    return m_PixelAccessor.Get( input, &input  - m_Begin );
    }

private:
  TPixelAccessor m_PixelAccessor; // The pixel accessor
  TInternalType *m_Begin; // Begin of the buffer
};

}

#endif    
