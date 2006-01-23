/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstrainedValueDifferenceImageFilter.h
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstrainedValueDifferenceImageFilter_h
#define __itkConstrainedValueDifferenceImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
  
/** \class ConstrainedValueDifferenceImageFilter
 * \brief Implements pixel-wise the computation of constrained value difference.
 *
 * This filter is parametrized over the types of the two 
 * input images and the type of the output image. 
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter will walk over all the pixels in the two input images, and for
 * each one of them it will do the following: 
 *
 * - cast the input 1 pixel value to \c double 
 * - cast the input 2 pixel value to \c double 
 * - compute the difference of the two pixel values
 * - compute the constrained value (constrained to be greater than or equal to the minimum value of the
 *   input 1 pixel type
 * - cast the \c double value resulting from \c the constrained value to the pixel type of the output image 
 * - store the casted value into the output image.
 * 
 * The filter expect all images to have the same dimension 
 * (e.g. all 2D, or all 3D, or all ND)
 * 
 * \author Lino Ramirez. Dept. of Electrical and Computer Engineering. University of Alberta. Canada
 *
 * \ingroup IntensityImageFilters Multithreaded
 */
namespace Functor {  
  
template< class TInput1, class TInput2, class TOutput>
class ConstrainedValueDifference
{
public:
  ConstrainedValueDifference() {};
  ~ConstrainedValueDifference() {};
  bool operator!=( const ConstrainedValueDifference & ) const
  {
    return false;
  }
  bool operator==( const ConstrainedValueDifference & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, 
                             const TInput2 & B)
  {
    const double dA = static_cast<double>( A );
    const double dB = static_cast<double>( B );
    const double diff = dA - dB;
    const double cdiff = ( diff > NumericTraits<TOutput>::min() ) ? diff : NumericTraits<TOutput>::min(); 
    return static_cast<TOutput>( cdiff );
  }
}; 
}

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT ConstrainedValueDifferenceImageFilter :
    public
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                         Functor::ConstrainedValueDifference< 
  typename TInputImage1::PixelType, 
  typename TInputImage2::PixelType,
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ConstrainedValueDifferenceImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                                   Functor::ConstrainedValueDifference< 
    typename TInputImage1::PixelType, 
    typename TInputImage2::PixelType,
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  ConstrainedValueDifferenceImageFilter() {}
  virtual ~ConstrainedValueDifferenceImageFilter() {}

private:
  ConstrainedValueDifferenceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
