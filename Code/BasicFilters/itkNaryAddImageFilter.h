/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryAddImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNaryAddImageFilter_h
#define __itkNaryAddImageFilter_h

#include "itkNaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class NaryAddImageFilter
 * \brief Implements an operator for pixel-wise addition of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 1 image must have a valid defintion of
 * the operator+ with a pixel type of the image 2. This condition is 
 * required because internally this filter will perform the operation
 *
 *        pixel_from_image_1 + pixel_from_image_2
 *
 * Additionally the type resulting from the sum, will be cast to
 * the pixel type of the output image.
 * 
 * The total operation over one pixel will be
 *
 *  output_pixel = static_cast<OutputPixelType>( input1_pixel + input2_pixel )
 *
 * For example, this filter could be used directly for adding images whose
 * pixels are vectors of the same dimension, and to store the resulting vector
 * in an output image of vector pixels.
 *
 * \warning No numeric overflow checking is performed in this filter.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

namespace Functor {  
  
template< class TInput, class TOutput >
class Add1
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;
  Add1() {}
  ~Add1() {}
  inline TOutput operator()( const TOutput & A, const TInput & B)
  {
    const AccumulatorType sum = A;
    return static_cast<TOutput>( sum + B );
  }
  bool operator != (const Add1&) const
  {
    return false;
  }
}; 
}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT NaryAddImageFilter :
    public
NaryFunctorImageFilter<TInputImage,TOutputImage, 
                       Functor::Add1<  typename TInputImage::PixelType, 
                                       typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef NaryAddImageFilter  Self;
  typedef NaryFunctorImageFilter<TInputImage,TOutputImage, 
                                 Functor::Add1< typename TInputImage::PixelType, 
                                                typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  NaryAddImageFilter() {}
  virtual ~NaryAddImageFilter() {}

private:
  NaryAddImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
