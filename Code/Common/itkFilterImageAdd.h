/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAdd.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageAdd_h
#define __itkFilterImageAdd_h

#include "itkFilterImageBinary.h"

namespace itk
{
  
/** \class FilterAdd
 * \brief Implements an operator for pixel-wise addition of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 */

namespace function {  
  
  template< class TInput1, class TInput2, class TOutput>
  class Add2
  {
  public:
    Add2() {};
    ~Add2() {};
    inline TOutput operator()( const TInput1 & A, const TInput2 & B)
    {
      return (TOutput)(A + B);
    }
  }; 

}

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT FilterImageAdd :
    public
    FilterImageBinary<TInputImage1,TInputImage2,TOutputImage, 
    function::Add2< 
              typename TInputImage1::PixelType, 
              typename TInputImage2::PixelType,
              typename TOutputImage::PixelType >   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageAdd  Self;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  FilterImageAdd() {}
  virtual ~FilterImageAdd() {}
  FilterImageAdd(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
