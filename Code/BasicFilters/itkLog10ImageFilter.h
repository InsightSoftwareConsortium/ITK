/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLog10ImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkLog10ImageFilter_h
#define __itkLog10ImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class Log10ImageFilter
 * \brief Computes the log10(x) pixel-wise
 *
 */

namespace Function {  
  
  template< class TInput, class TOutput>
  class Log10
  {
  public:
    Log10() {};
    ~Log10() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)log10((double)A);
    }
  }; 

}
// Wrap: Log10ImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: Log10ImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT Log10ImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    Function::Log10< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Log10ImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    Function::Log10< 
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

  Log10ImageFilter() {}
  virtual ~Log10ImageFilter() {}
  Log10ImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
