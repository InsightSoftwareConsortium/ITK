/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAtanImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAtanImageFilter_h
#define __itkAtanImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class AtanImageFilter
 * \brief Computes the atan(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Atan
  {
  public:
    Atan() {};
    ~Atan() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)atan(A);
    }
  }; 

}
// Wrap: AtanImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: AtanImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AtanImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Atan< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AtanImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Atan< 
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

  AtanImageFilter() {}
  virtual ~AtanImageFilter() {}
  AtanImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
