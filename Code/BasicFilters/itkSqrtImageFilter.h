/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSqrtImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSqrtImageFilter_h
#define __itkSqrtImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class SqrtImageFilter
 * \brief Computes the sqrt(x) pixel-wise
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function {  
  
template< class TInput, class TOutput>
class Sqrt
{
public:
  Sqrt() {}
  ~Sqrt() {}
  inline TOutput operator()( const TInput & A )
  {
    return (TOutput)sqrt((double)A);
  }
}; 
}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SqrtImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Function::Sqrt< typename TInputImage::PixelType, 
                                        typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef SqrtImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Function::Sqrt< typename TInputImage::PixelType, 
                                                  typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  SqrtImageFilter() {}
  virtual ~SqrtImageFilter() {}

private:
  SqrtImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
