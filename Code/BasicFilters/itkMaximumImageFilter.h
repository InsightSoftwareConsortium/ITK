/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaximumImageFilter_h
#define __itkMaximumImageFilter_h

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{
  
/** \class MaximumImageFilter
 * \brief Implements a pixel-wise operator Max(a,b) between two images.
 *
 * The pixel values of the output image are the maximum between the 
 * corresponding pixels of the two input images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function {  

template< class TInput1, class TInput2, class TOutput>
class Maximum
{
public:
  Maximum() {}
  ~Maximum() {}
  bool operator!=( const Maximum & ) const
  {
    return false;
  }
  bool operator==( const Maximum & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  { 
   if(A > B)
     {
     return static_cast<TOutput>(A);
     }
   else
     {
     return static_cast<TOutput>(B);
     }
  } 
};
}

template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT MaximumImageFilter :
    public
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                         Function::Maximum< 
  typename TInputImage1::PixelType, 
  typename TInputImage2::PixelType,
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef MaximumImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                                   Function::Maximum< 
    typename TInputImage1::PixelType, 
    typename TInputImage2::PixelType,
    typename TOutputImage::PixelType>   
  > Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(Input1ConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage1::PixelType,
                          typename TOutputImage::PixelType>));
  itkConceptMacro(Input2ConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage2::PixelType,
                          typename TOutputImage::PixelType>));
  itkConceptMacro(Input1GreaterThanInput2Check,
    (Concept::GreaterThanComparable<typename TInputImage1::PixelType,
                                    typename TInputImage2::PixelType>));
  /** End concept checking */
#endif

protected:
  MaximumImageFilter() {}
  virtual ~MaximumImageFilter() {}

private:
  MaximumImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
