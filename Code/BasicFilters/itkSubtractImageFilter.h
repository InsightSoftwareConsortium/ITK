/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubtractImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSubtractImageFilter_h
#define __itkSubtractImageFilter_h

#include "itkBinaryImageFilter.h"

namespace itk
{
  
/** \class SubtractImageFilter
 * \brief Implements an operator for pixel-wise substraction of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace Function {  
  
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
class ITK_EXPORT SubtractImageFilter :
    public
    BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
    Function::Sub2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SubtractImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
    Function::Sub2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
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

  SubtractImageFilter() {}
  virtual ~SubtractImageFilter() {}
  SubtractImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
