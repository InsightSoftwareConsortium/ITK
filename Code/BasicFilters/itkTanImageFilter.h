/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTanImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTanImageFilter_h
#define __itkTanImageFilter_h

#include "itkUnaryImageFilter.h"
#include "cmath"

namespace itk
{
  
/** \class TanImageFilter
 * \brief Computes the tan(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Tan
  {
  public:
    Tan() {};
    ~Tan() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)tan(A);
    }
  }; 

}
// Wrap: TanImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: TanImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT TanImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Tan< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TanImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Tan< 
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

  TanImageFilter() {}
  virtual ~TanImageFilter() {}
  TanImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
