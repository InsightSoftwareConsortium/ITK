/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAsinImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAsinImageFilter_h
#define __itkAsinImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class AsinImageFilter
 * \brief Computes the asin(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Asin
  {
  public:
    Asin() {};
    ~Asin() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)asin(A);
    }
  }; 

}
// Wrap: AsinImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: AsinImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AsinImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Asin< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AsinImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Asin< 
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

  AsinImageFilter() {}
  virtual ~AsinImageFilter() {}
  AsinImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
