/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAcosImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAcosImageFilter_h
#define __itkAcosImageFilter_h

#include "itkUnaryImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class AcosImageFilter
 * \brief Computes the acos(x) pixel-wise
 *
 */

namespace function {  
  
  template< class TInput, class TOutput>
  class Acos
  {
  public:
    Acos() {};
    ~Acos() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)acos((double)A);
    }
  }; 

}
// Wrap: AcosImageFilter<$Image,$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: AcosImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AcosImageFilter :
    public
    UnaryImageFilter<TInputImage,TOutputImage, 
    function::Acos< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AcosImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryImageFilter<TInputImage,TOutputImage, 
    function::Acos< 
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

  AcosImageFilter() {}
  virtual ~AcosImageFilter() {}
  AcosImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
