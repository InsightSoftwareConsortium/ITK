/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSub.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageSub_h
#define __itkFilterImageSub_h

#include "itkFilterImageBinary.h"

namespace itk
{
  
/** \class FilterSub
 * \brief Implements an operator for pixel-wise substraction of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TOutput>
  class Sub2
  {
  public:
    Sub2() {};
    ~Sub2() {};
    inline TOutput operator()( const TInput1 & A, const TInput2 & B)
    {
      return (TOutput)(A - B);
    }
  }; 

}

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT FilterImageSub :
    public
    FilterImageBinary<TInputImage1,TInputImage2,TOutputImage, 
    function::Sub2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
              typename TOutputImage::PixelType >   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageSub  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageBinary<TInputImage1,TInputImage2,TOutputImage, 
    function::Sub2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
              typename TOutputImage::PixelType >   >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  FilterImageSub() {}
  virtual ~FilterImageSub() {}
  FilterImageSub(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
