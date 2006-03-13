/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundedReciprocalImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBoundedReciprocalImageFilter_h
#define __itkBoundedReciprocalImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class BoundedReciprocalImageFilter
 *
 * \brief Computes 1/(1+x) for each pixel in the image
 * 
 * The filter expect both the input and output images to have the same 
 * number of dimensions, and both of a scalar image type.
 *
 */
namespace Functor {  
  
template< class TInput, class TOutput>
class BoundedReciprocal
{
public:
  BoundedReciprocal() {};
  ~BoundedReciprocal() {};
  bool operator!=( const BoundedReciprocal & ) const
  {
    return false;
  }
  bool operator==( const BoundedReciprocal & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( 1.0 / ( 1.0 +  static_cast<double>(A) ) );
  }
};
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT BoundedReciprocalImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::BoundedReciprocal< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef BoundedReciprocalImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::BoundedReciprocal< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToDoubleCheck,
    (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck,
    (Concept::Convertible<double, typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  BoundedReciprocalImageFilter() {}
  virtual ~BoundedReciprocalImageFilter() {}

private:
  BoundedReciprocalImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


} // end namespace itk


#endif
