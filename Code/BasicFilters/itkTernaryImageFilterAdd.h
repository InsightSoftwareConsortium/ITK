/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryImageFilterAdd.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTernaryImageFilterAdd_h
#define __itkTernaryImageFilterAdd_h

#include "itkTernaryImageFilter.h"

namespace itk
{
  
/** \class TernaryImageFilterAdd
 * \brief Implements pixel-wise addition of three images.
 *
 * This class is parametrized over the types of the three 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TInput3, class TOutput>
  class Add3
  {
  public:
    Add3() {};
    ~Add3() {};
    inline TOutput operator()( const TInput1 & A, 
                               const TInput2 & B,
                               const TInput3 & C)
    {
      return (TOutput)(A + B + C);
    }
  }; 

}

template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage>
class ITK_EXPORT TernaryImageFilterAdd :
    public
    TernaryImageFilter<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
            function::Add3< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TernaryImageFilterAdd  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef TernaryImageFilter<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
                      function::Add3< 
                      typename TInputImage1::PixelType,
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType>   >  Superclass;

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

  TernaryImageFilterAdd() {}
  virtual ~TernaryImageFilterAdd() {}
  TernaryImageFilterAdd(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
