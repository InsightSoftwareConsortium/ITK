/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCosImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCosImageFilter_h
#define __itkCosImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class CosImageFilter
 * \brief Computes the cos(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Cos
  {
  public:
    Cos() {};
    ~Cos() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)cos(A);
    }
  }; 

}
// Wrap: CosImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: CosImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT CosImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Cos< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CosImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Cos< 
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

  CosImageFilter() {}
  virtual ~CosImageFilter() {}
  CosImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
