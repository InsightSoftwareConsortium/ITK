/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSinImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSinImageFilter_h
#define __itkSinImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class SinImageFilter
 * \brief Computes the sin(x) pixel-wise
 *
 */

namespace Function {  
  
  template< class TInput, class TOutput>
  class Sin
  {
  public:
    Sin() {};
    ~Sin() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)sin((double)A);
    }
  }; 

}
// Wrap: SinImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: SinImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SinImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    Function::Sin< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SinImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    Function::Sin< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   
                >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  SinImageFilter() {}
  virtual ~SinImageFilter() {}
  SinImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
