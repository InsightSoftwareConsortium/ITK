/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkLogImageFilter_h
#define __itkLogImageFilter_h

#include "itkUnaryImageFilter.h"
#include "cmath"

namespace itk
{
  
/** \class LogImageFilter
 * \brief Computes the log(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Log
  {
  public:
    Log() {};
    ~Log() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)log(A);
    }
  }; 

}
// Wrap: LogImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: LogImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT LogImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Log< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LogImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Log< 
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

  LogImageFilter() {}
  virtual ~LogImageFilter() {}
  LogImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
