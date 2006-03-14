/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComplexToPhaseImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkComplexToPhaseImageFilter_h
#define __itkComplexToPhaseImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class ComplexToPhaseImageFilter
 * \brief Computes pixel-wise the modulus of a complex image.
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function {  
  
template< class TInput, class TOutput>
class ComplexToPhase
{
public:
  ComplexToPhase() {}
  ~ComplexToPhase() {}
  bool operator!=( const ComplexToPhase & ) const
  {
    return false;
  }
  bool operator==( const ComplexToPhase & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  { return (TOutput)( atan2( A.imag(), A.real() ) );}
}; 
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ComplexToPhaseImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Function::ComplexToPhase< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ComplexToPhaseImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Function::ComplexToPhase< typename TInputImage::PixelType, 
                                                 typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage::PixelType::value_type,
                          typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  ComplexToPhaseImageFilter() {}
  virtual ~ComplexToPhaseImageFilter() {}

private:
  ComplexToPhaseImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
