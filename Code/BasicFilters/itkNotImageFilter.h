/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNotImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNotImageFilter_h
#define __itkNotImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class NotImageFilter
 * \brief Implements the NOT logical operator pixel-wise on an image.
 *
 * This class is parametrized over the types of an 
 * input image and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Since the logical NOT operation is only defined in C++ for integer
 * types, the images passed to this filter must comply with the requirement
 * of using integer pixel type. 
 * 
 * The total operation over one pixel will be
 *
 *  output_pixel = static_cast<OutputPixelType>( !input_pixel )
 *
 * Where "!" is the unary NOT operator in C++.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TOutput=TInput >
class NOT
{
public:
  NOT() {};
 ~NOT() {};
  bool operator!=( const NOT & other ) const
  {
    return false;
  }
  bool operator==( const NOT & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( !A );
  }
}; 

}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT NotImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                         Functor::NOT< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >


{
public:
  /** Standard class typedefs. */
  typedef NotImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                   Functor::NOT< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  NotImageFilter() {}
  virtual ~NotImageFilter() {}

private:
  NotImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
