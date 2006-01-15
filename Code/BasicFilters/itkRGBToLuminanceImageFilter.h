/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBToLuminanceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBToLuminanceImageFilter_h
#define __itkRGBToLuminanceImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class RGBToLuminanceImageFilter
 * \brief Converts an RGB image into a grayscale image.
 * 
 * This filters converts an RGB image into a Luminance on by computing
 * pixel-wise a linear combination on the Red, Green and Blue channels. The
 * pixel type of the input image must have a GetLuminance() method. This is the
 * case of the itk::RGBPixel class.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function {  
  
template< class TInput, class TOutput>
class RGBToLuminance
{
public:
  typedef typename TInput::ComponentType      ComponentType;
  typedef typename itk::NumericTraits< ComponentType >::RealType  RealType;

  RGBToLuminance() {}
  ~RGBToLuminance() {}
  bool operator!=( const RGBToLuminance & other ) const
  {
    return false;
  }
  bool operator==( const RGBToLuminance & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  { return static_cast<TOutput>( A.GetLuminance() ); }
}; 
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RGBToLuminanceImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Function::RGBToLuminance< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef RGBToLuminanceImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Function::RGBToLuminance< typename TInputImage::PixelType, 
                                                 typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  RGBToLuminanceImageFilter() {}
  virtual ~RGBToLuminanceImageFilter() {}

private:
  RGBToLuminanceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
