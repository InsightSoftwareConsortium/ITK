/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpNegativeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExpNegativeImageFilter_h
#define __itkExpNegativeImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
  
/** \class ExpNegativeImageFilter
 * \brief Computes the exp(x) pixel-wise
 *
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */

namespace Function {  
  template< class TInput, class TOutput>
  class ExpNegative
  {
  public:
    ExpNegative() {};
    ~ExpNegative() {};
    inline TOutput operator()( const TInput & A )
    {
      return static_cast<TOutput>( exp( - static_cast<double>(A) ) );
    }
  }; 
}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExpNegativeImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::ExpNegative< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ExpNegativeImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::ExpNegative< typename TInputImage::PixelType, 
                   typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  ExpNegativeImageFilter() {}
  virtual ~ExpNegativeImageFilter() {}

private:
  ExpNegativeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif
