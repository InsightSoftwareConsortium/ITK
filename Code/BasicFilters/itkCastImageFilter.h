/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCastImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCastImageFilter_h
#define __itkCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class CastImageFilter
 *
 * \brief Casts input pixels to output pixel type.
 *
 * This filter is templated over the input image type
 * and the output image type.
 * 
 * The filter expect both images to have the same number of dimensions.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
  template< class TInput, class TOutput>
  class Cast
  {
  public:
    Cast() {};
    ~Cast() {};
    inline TOutput operator()( const TInput & A )
    {
      return static_cast<TOutput>( A );
    }
  };
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT CastImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::Cast< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef CastImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::Cast< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   
                >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(CastImageFilter, UnaryFunctorImageFilter);

 protected:
  CastImageFilter() {}
  virtual ~CastImageFilter() {}

private:
  CastImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


} // end namespace itk


#endif
