/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Modulus:    itkFilterImageTernaryModulus.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageTernaryModulus_h
#define __itkFilterImageTernaryModulus_h

#include "itkFilterImageTernary.h"
#include <cmath>

namespace itk
{
  
/** \class FilterTernaryModulus
 * \brief Implements pixel-wise addition of three images.
 *
 * This class is parametrized over the types of the three 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TInput3, class TOutput>
  class Modulus3
  {
  public:
    Modulus3() {};
    ~Modulus3() {};
    inline TOutput operator()( const TInput1 & A, 
                               const TInput2 & B,
                               const TInput3 & C)
    {
      return (TOutput) sqrt( (TOutput)(A*A + B*B + C*C) );
    }
  }; 

}



template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage>
class ITK_EXPORT FilterImageTernaryModulus :
    public
    FilterImageTernary<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
            function::Modulus3< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType >   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageTernaryModulus  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageTernary<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
                      function::Modulus3< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType >   >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  FilterImageTernaryModulus() {}
  virtual ~FilterImageTernaryModulus() {}
  FilterImageTernaryModulus(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
