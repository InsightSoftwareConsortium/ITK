/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSqrtImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSqrtImageFilter_h
#define __itkSqrtImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class SqrtImageFilter
 * \brief Computes the sqrt(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Sqrt
  {
  public:
    Sqrt() {};
    ~Sqrt() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)sqrt((double)A);
    }
  }; 

}
// Wrap: SqrtImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: SqrtImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SqrtImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Sqrt< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SqrtImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Sqrt< 
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

  SqrtImageFilter() {}
  virtual ~SqrtImageFilter() {}
  SqrtImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
