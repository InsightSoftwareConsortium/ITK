/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientToMagnitudeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientToMagnitudeImageFilter_h
#define __itkGradientToMagnitudeImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class GradientToMagnitudeImageFilter
 *
 * \brief Take an image of vectors as input and produce an image with the
 *  magnitude of those vectors.
 *
 * The filter expects the input image pixel type to be a vector and 
 * the output image pixel type to be a scalar.
 *
 * This filter assumes that the PixelType of the input image is a VectorType
 * that provides a GetNorm() method.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

namespace Functor {  
  
template< class TInput, class TOutput>
class GradientMagnitude
{
public:
  GradientMagnitude() {}
  ~GradientMagnitude() {}

  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( A.GetNorm() );
  }
}; 
}
 
template <class TInputImage, class TOutputImage>
class ITK_EXPORT GradientToMagnitudeImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::GradientMagnitude< typename TInputImage::PixelType, 
                                                    typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef GradientToMagnitudeImageFilter Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::GradientMagnitude< typename TInputImage::PixelType, 
                                                              typename TOutputImage::PixelType> > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  GradientToMagnitudeImageFilter() {}
  virtual ~GradientToMagnitudeImageFilter() {}
    
private:
  GradientToMagnitudeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};
 
} // end namespace itk


#endif
