/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageTernaryModule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageTernaryModule_h
#define __itkFilterImageTernaryModule_h

#include "itkFilterImageTernary.h"
#include <cmath>

namespace itk
{
  
/** \class FilterTernaryModule
 * \brief Implements pixel-wise addition of three images.
 *
 * This class is parametrized over the types of the three 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TInput3, class TOutput>
  class Module3
  {
  public:
    Module3() {};
    ~Module3() {};
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
class ITK_EXPORT FilterImageTernaryModule :
    public
    FilterImageTernary<TInputImage1,TInputImage2,
                      TInputImage3,TOutputImage, 
            function::Module3< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TInputImage3::PixelType,
                      typename TOutputImage::PixelType >   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageTernaryModule  Self;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  FilterImageTernaryModule() {}
  virtual ~FilterImageTernaryModule() {}
  FilterImageTernaryModule(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
