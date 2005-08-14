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
template <class TInternalType, class TExternalType, class TPixelAccessor  >
class ITK_EXPORT DefaultPixelAccessorFunctor
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

  /** Set the pointer index to the start of the buffer. 
   * The method exists to maintain consistency in the API of the 
   * DefaultPixelAccessorFunctor and the DefaultVectorPixelAccessorFunctor. */
  inline void SetBegin( const TInternalType *begin ) {};
  
  /** Set output using the value in input */
  inline void Set( TInternalType & output, const TExternalType &input ) const
    {
    m_PixelAccessor.Set( output, input );
    }

  /** Get the value from input */
  inline TExternalType & Get( TInternalType &input ) const
    {
    return m_PixelAccessor.Get( input );
    }

  /** Get a const reference to the pixel. */
  inline const TExternalType & Get( const TInternalType & input ) const
    {
    return m_PixelAccessor.Get( input );
    }
  
private:
  TPixelAccessor m_PixelAccessor; // The pixel accessor
};

}

#endif    
