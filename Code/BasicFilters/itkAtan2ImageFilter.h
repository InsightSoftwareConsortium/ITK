/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAtan2ImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAtan2ImageFilter_h
#define __itkAtan2ImageFilter_h

#include "itkBinaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class Atan2ImageFilter
 * \brief Computes arctangent pixel-wise from two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace Function {  
  
  template< class TInput1, class TInput2, class TOutput>
  class Atan2
  {
  public:
    Atan2() {};
    ~Atan2() {};
    inline TOutput operator()( const TInput1 & A, const TInput2 & B)
    {
      return (TOutput)atan2((double) A,(double) B);
    }
  }; 

}
// Wrap: Atan2ImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: Atan2ImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT Atan2ImageFilter :
    public
    BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
    Function::Atan2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Atan2ImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
    Function::Atan2< 
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

  Atan2ImageFilter() {}
  virtual ~Atan2ImageFilter() {}
  Atan2ImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
