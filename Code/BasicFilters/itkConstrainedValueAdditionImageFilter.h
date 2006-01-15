/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstrainedValueAdditionImageFilter.h
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstrainedValueAdditionImageFilter_h
#define __itkConstrainedValueAdditionImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
  
/** \class ConstrainedValueAdditionImageFilter
 * \brief Implements pixel-wise the computation of constrained value addition.
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
 * - compute the addition of the two pixel values
 * - compute the constrained value (constrained to be less than or equal to the maximum value of the
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
class ConstrainedValueAddition
{
public:
  ConstrainedValueAddition() {};
  ~ConstrainedValueAddition() {};
  bool operator!=( const ConstrainedValueAddition & other ) const
  {
    return false;
  }
  bool operator==( const ConstrainedValueAddition & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, 
                             const TInput2 & B)
  {
    const double dA = static_cast<double>( A );
    const double dB = static_cast<double>( B );
    const double add = dA + dB;
    const double cadd = ( add < NumericTraits<TOutput>::max() ) ? add : NumericTraits<TOutput>::max(); 
    return static_cast<TOutput>( cadd );
  }
}; 
}

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT ConstrainedValueAdditionImageFilter :
    public
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                         Functor::ConstrainedValueAddition< 
  typename TInputImage1::PixelType, 
  typename TInputImage2::PixelType,
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ConstrainedValueAdditionImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                                   Functor::ConstrainedValueAddition< 
    typename TInputImage1::PixelType, 
    typename TInputImage2::PixelType,
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  ConstrainedValueAdditionImageFilter() {}
  virtual ~ConstrainedValueAdditionImageFilter() {}

private:
  ConstrainedValueAdditionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
