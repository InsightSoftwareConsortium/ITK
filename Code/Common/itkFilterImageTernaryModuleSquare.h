/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageTernaryModuleSquare.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageTernaryModuleSquare_h
#define __itkFilterImageTernaryModuleSquare_h

#include "itkFilterImageTernary.h"

namespace itk
{
  
/** \class FilterTernaryModuleSquare
 * \brief Implements pixel-wise addition of three images.
 *
 * This class is parametrized over the types of the three 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TInput3, class TOutput>
  class ModuleSquare3
  {
  public:
    ModuleSquare3() {};
    ~ModuleSquare3() {};
    inline TOutput operator()( const TInput1 & A, 
                               const TInput2 & B,
                               const TInput3 & C)
    {
      return (TOutput)(A*A + B*B + C*C);
    }
  }; 

}



template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage>
class ITK_EXPORT FilterImageTernaryModuleSquare :
    public
    FilterImageTernary<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
            function::ModuleSquare3< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType >   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageTernaryModuleSquare  Self;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  FilterImageTernaryModuleSquare() {}
  virtual ~FilterImageTernaryModuleSquare() {}
  FilterImageTernaryModuleSquare(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
