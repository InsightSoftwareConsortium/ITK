/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkExpImageFilter_h
#define __itkExpImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class ExpImageFilter
 * \brief Computes the exp(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Exp
  {
  public:
    Exp() {};
    ~Exp() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)exp(A);
    }
  }; 

}
// Wrap: ExpImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: ExpImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExpImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Exp< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ExpImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Exp< 
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

  ExpImageFilter() {}
  virtual ~ExpImageFilter() {}
  ExpImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
